#include "DebugGen.h"

#include <codecvt>
#include <llvm/ADT/SmallString.h>
#include <llvm/IR/Module.h>

#include "Context.h"
#include "SourceFile.h"
#include "GenTypes.h"

llvm::DenseMap<acorn::Type*, llvm::DIType*> acorn::DebugInfoEmitter::di_cached_types;

acorn::DebugInfoEmitter::DebugInfoEmitter(Context& context, SourceFile* file)
    : builder(llvm::DIBuilder(context.get_ll_module())),
	  context(context),
	  file(file)
{
}

void acorn::DebugInfoEmitter::emit_function(Func* func) {
	// Ensure the compilation unit exists for this function.
	emit_file(func->file);

	llvm::DIScope* di_scope;
	if (func->structn) {
		di_scope = emit_type(func->structn->struct_type);
	} else {
		di_scope = di_unit;
	}

	size_t line_number       = func->file->line_table.get_line_number(func->loc);
	size_t scope_line_number = func->file->line_table.get_line_number(func->scope->loc);

	llvm::SmallVector<llvm::Metadata*> di_func_types;
	di_func_types.push_back(emit_type(func->return_type));

	// Needs to know about the 'this' pointer type.
	if (func->structn) {
		auto this_type = context.type_table.get_ptr_type(func->structn->struct_type);
		auto di_this_type = builder.createObjectPointerType(emit_type(this_type));
		di_func_types.push_back(di_this_type);
	}

	for (Var* param : func->params) {
		di_func_types.push_back(emit_type(param->type));
	}

	auto di_func_type = builder.createSubroutineType(builder.getOrCreateTypeArray(di_func_types));

	auto di_func = builder.createFunction(
		di_scope,
		func->name.to_string(),
		func->ll_func->getName(), // Linkage name
		di_unit->getFile(),
		static_cast<unsigned>(line_number),
		di_func_type,
		static_cast<unsigned>(scope_line_number),
		llvm::DINode::DIFlags::FlagPrototyped,
		llvm::DISubprogram::DISPFlags::SPFlagDefinition,
		nullptr,
		nullptr // DIForwardDeclaredFunc // Forward declaration
	);

	func->ll_func->setSubprogram(di_func);
	di_lexical_scopes.push_back(di_func);
}

void acorn::DebugInfoEmitter::emit_function_end(Func* func) {
	llvm::DISubprogram* di_func = func->ll_func->getSubprogram();

	// !! IF THE PROGRAM HAS A FORWARD DECLARATION YOU STILL HAVE TO FINALIZE IT OR THE RETAINED
	// NODES WILL BE TEMPORARY !!
	if (llvm::DISubprogram* di_forward_decl = di_func->getDeclaration()) {
		builder.finalizeSubprogram(di_forward_decl);
	}
	builder.finalizeSubprogram(di_func);
}

void acorn::DebugInfoEmitter::emit_struct_this_variable(llvm::Value* ll_this, Func* func, llvm::IRBuilder<>& ir_builder) {
	auto di_scope = func->ll_func->getSubprogram();

	auto struct_type = func->structn->struct_type;
	auto this_type = context.type_table.get_ptr_type(struct_type);

	auto di_variable = builder.createParameterVariable(
		di_scope,
		"this",
		1, // Argument number
		di_unit->getFile(),
		0, // TODO: Line number?
		emit_type(this_type),
		true, // Always perserve
		llvm::DINode::FlagArtificial | llvm::DINode::FlagObjectPointer
	);

	auto di_location = llvm::DILocation::get(context.get_ll_context(), 0, 0, di_scope);

	builder.insertDeclare(
		ll_this,
		di_variable,
		builder.createExpression(),
		di_location,
		ir_builder.GetInsertBlock()
	);
}

void acorn::DebugInfoEmitter::emit_location(llvm::IRBuilder<>& ir_builder, SourceLoc location) {
	auto* ll_last_instruction = &ir_builder.GetInsertBlock()->back();
	emit_location(ll_last_instruction, location);
}

void acorn::DebugInfoEmitter::emit_location(llvm::Instruction* ll_instruction, SourceLoc location) {

	auto [line_number, column_number] = file->line_table.get_line_and_column_number(location);

	llvm::DIScope* di_scope = di_lexical_scopes.back();
	auto di_location = llvm::DILocation::get(
		context.get_ll_context(),
		static_cast<unsigned>(line_number),
		// Emission of column number seems to confuse the debugger and cause it to make multiple steps for an instruction on a single line.
		// This is probably because it breaks once per unique location and adding the column number makes the location non-unique to the line
		// its on.
		0, //column_number,
		di_scope
	);

	ll_instruction->setDebugLoc(di_location);
}

void acorn::DebugInfoEmitter::emit_location_at_last_statement(llvm::IRBuilder<>& ir_builder) {
	emit_location(ir_builder, last_stmt->loc);
}

void acorn::DebugInfoEmitter::emit_function_variable(Var* var, llvm::IRBuilder<>& ir_builder) {
	auto di_scope = di_lexical_scopes.back();

	auto [line_number, column_number] = var->file->line_table.get_line_and_column_number(var->loc);

	auto di_var = builder.createAutoVariable(
		di_scope,
		var->name.to_string(),
		di_unit->getFile(),
		static_cast<unsigned>(line_number),
		emit_type(var->type),
		true // Always perserve
	);

	auto di_location = llvm::DILocation::get(
		context.get_ll_context(),
		static_cast<unsigned>(line_number),
		static_cast<unsigned>(column_number),
		di_scope
	);

	builder.insertDeclare(
		var->ll_address,
		di_var,
		builder.createExpression(),
		di_location,
		ir_builder.GetInsertBlock()
	);
}

void acorn::DebugInfoEmitter::emit_global_variable(Var* global) {
	// Ensure the compilation unit exists for this global.
	emit_file(global->file);

	size_t line_number = global->file->line_table.get_line_number(global->loc);

	auto di_global = builder.createGlobalVariableExpression(
		di_unit,
		global->name.to_string(),
		global->ll_address->getName(), // Linkage name
		di_unit->getFile(),
		static_cast<unsigned>(line_number),
		emit_type(global->type),
		false, // TODO: is local?
		true // Is defined
	);

	auto ll_global = llvm::cast<llvm::GlobalVariable>(global->ll_address);
	ll_global->addDebugInfo(di_global);
}

void acorn::DebugInfoEmitter::emit_scope_start(SourceLoc loc) {

	auto [line_number, column_number] = file->line_table.get_line_and_column_number(loc);

	auto di_lexical_block = builder.createLexicalBlock(
		di_lexical_scopes.back(),
		di_unit->getFile(),
		static_cast<unsigned>(line_number),
		static_cast<unsigned>(column_number)
	);
	di_lexical_scopes.push_back(di_lexical_block);
}

void acorn::DebugInfoEmitter::emit_scope_end() {
	di_lexical_scopes.pop_back();
}

void acorn::DebugInfoEmitter::finalize() {
	if (di_unit) { // If is possible no code was generated so we have to make sure there is a di_unit.
		builder.finalize();
	}
}

void acorn::DebugInfoEmitter::clear_cache() {
	di_cached_types.clear();
}

void acorn::DebugInfoEmitter::emit_file(SourceFile* file) {
    if (di_unit) {
        return; // Exit early because the debug info has already been generated.
    }

	auto& wfull_path = file->full_path;

	auto itr = wfull_path.find_last_of('/');
	bool has_last_slash = itr != std::wstring::npos;

	auto wfile_name = has_last_slash ? wfull_path.substr(itr + 1) : wfull_path;
	auto wdirectory = wfull_path.substr(0, wfull_path.size() - wfile_name.size());
	if (wdirectory.ends_with('/')) {
		wdirectory = wdirectory.substr(0, wdirectory.size() - 1);
	}

	// TODO: validate that converting to ascii works here.
	std::wstring_convert<std::codecvt_utf8<wchar_t>> wconverter;
	auto file_name = wconverter.to_bytes(wfile_name);
	auto directory = wconverter.to_bytes(wdirectory);


	// TODO: Checksums can be used as a way to verify that the source code
	//       is the same as the executable's debug info for that source code.
	//       Also, this should be able to be disabled as checksums can slow down
	//       compilation time.
	std::optional<llvm::DIFile::ChecksumInfo<llvm::StringRef>> checksum_info;
	llvm::SmallString<64> checksum;
	std::optional<llvm::DIFile::ChecksumKind> checksum_kind;
	if (checksum_kind)
		checksum_info.emplace(*checksum_kind, checksum);

	llvm::StringRef debug_flags = "";

	llvm::DIFile* di_unit_file = builder.createFile(file_name, directory, checksum_info);

    di_unit = builder.createCompileUnit(
		//llvm::dwarf::DW_LANG_C99,
		// Using C++ 14 because it pretty closely matches our language
		// and can recognize things like the 'this' pointer.
		llvm::dwarf::DW_LANG_C_plus_plus_14,
		di_unit_file,
		"acorn compiler", // Producer
		false,            // TODO: Is Optimized
		debug_flags,
		0,
		llvm::StringRef(),
		llvm::DICompileUnit::DebugEmissionKind::FullDebug,
		0,
		false,
		false,
		llvm::DICompileUnit::DebugNameTableKind::None
	);
}

llvm::DIType* acorn::DebugInfoEmitter::emit_type(Type* type) {

	auto itr = di_cached_types.find(type);
	if (itr != di_cached_types.end()) {
		return itr->second;
	}

	auto make_basic_type = [this, type](const char* name, uint64_t bit_size, unsigned data_type) {
		llvm::DIType* di_type = builder.createBasicType(name, bit_size, data_type);
		di_cached_types.insert({ type, di_type });
		return di_type;
	};

	switch (type->get_kind()) {
	case TypeKind::Void:    return nullptr; // Yes this is correct it expects nullptr.
	case TypeKind::Int:     return make_basic_type("int"   , 32, llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int8:    return make_basic_type("int8"  , 8 , llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int16:   return make_basic_type("int16" , 16, llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int32:   return make_basic_type("int32" , 32, llvm::dwarf::DW_ATE_signed);
	case TypeKind::Int64:   return make_basic_type("int64" , 64, llvm::dwarf::DW_ATE_signed);
	case TypeKind::UInt8:   return make_basic_type("uint8" , 8 , llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt16:  return make_basic_type("uint16", 16, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt32:  return make_basic_type("uint32", 32, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::UInt64:  return make_basic_type("uint64", 64, llvm::dwarf::DW_ATE_unsigned);
	case TypeKind::ISize: {
		unsigned ptr_size_in_bits = context.get_ll_module().getDataLayout().getPointerSizeInBits();
		return make_basic_type("isize", ptr_size_in_bits, llvm::dwarf::DW_ATE_signed);
	}
	case TypeKind::USize: {
		unsigned ptr_size_in_bits = context.get_ll_module().getDataLayout().getPointerSizeInBits();
		return make_basic_type("isize", ptr_size_in_bits, llvm::dwarf::DW_ATE_unsigned);
	}
	case TypeKind::Char:    return make_basic_type("char"   , 8 , llvm::dwarf::DW_ATE_unsigned_char);
	case TypeKind::Char16:  return make_basic_type("char16" , 16, llvm::dwarf::DW_ATE_UTF);
	case TypeKind::Char32:  return make_basic_type("char32" , 32, llvm::dwarf::DW_ATE_UTF);
	case TypeKind::Float32: return make_basic_type("float32", 32, llvm::dwarf::DW_ATE_float);
	case TypeKind::Float64: return make_basic_type("float64", 64, llvm::dwarf::DW_ATE_float);
	case TypeKind::Bool:    return make_basic_type("bool"   , 8 , llvm::dwarf::DW_ATE_boolean);
	case TypeKind::Pointer: {
		unsigned ptr_size_in_bits = context.get_ll_module().getDataLayout().getPointerSizeInBits();

		auto ptr_type = static_cast<PointerType*>(type);
		auto di_elm_type = emit_type(ptr_type->get_elm_type());
		auto di_ptr_type = builder.createPointerType(
			di_elm_type,
			ptr_size_in_bits,
			0 // TODO: alignment
		);

		di_cached_types.insert({ type, di_ptr_type });
		return di_ptr_type;
	}
	case TypeKind::Array: {
		unsigned ptr_size_in_bits = context.get_ll_module().getDataLayout().getPointerSizeInBits();
		auto ll_length_type = llvm::Type::getIntNTy(context.get_ll_context(), ptr_size_in_bits);

		auto arr_type = static_cast<ArrayType*>(type);
		auto base_type = arr_type->get_base_type();
		auto di_base_type = emit_type(base_type);

		auto ll_base_type = gen_type(base_type, context.get_ll_context(), context.get_ll_module());
		uint64_t size_in_bits = arr_type->get_total_linear_length();
		size_in_bits *= context.get_ll_module().getDataLayout().getTypeAllocSizeInBits(ll_base_type);

		llvm::SmallVector<llvm::Metadata*> di_subscript_lengths;
		bool more_subscripts = false;
		do {

			auto di_subscript_length = llvm::ConstantAsMetadata::get(
				llvm::ConstantInt::get(ll_length_type, arr_type->get_length()));

			di_subscript_lengths.push_back(builder.getOrCreateSubrange(0, di_subscript_length));

			auto elm_type = arr_type->get_elm_type();
			more_subscripts = elm_type->is_array();
			if (more_subscripts) {
				arr_type = static_cast<ArrayType*>(elm_type);
			}
		} while (more_subscripts);

		auto di_arr_type = builder.createArrayType(
			size_in_bits,
			0, // TODO: Alignment
			di_base_type,
			builder.getOrCreateArray(di_subscript_lengths)
		);

		di_cached_types.insert({ type, di_arr_type });
		return di_arr_type;
	}
	case TypeKind::Slice: {
		auto slice_type = static_cast<SliceType*>(type);
		auto ll_slice_type = gen_type(slice_type, context.get_ll_context(), context.get_ll_module());
		auto ll_struct_type = llvm::cast<llvm::StructType>(ll_slice_type);

		auto ll_struct_layout = context.get_ll_module().getDataLayout().getStructLayout(ll_struct_type);
		uint64_t size_in_bits = ll_struct_layout->getSizeInBits();

		auto di_struct_type = builder.createStructType(
			nullptr,
			ll_struct_type->getName(),
			di_unit->getFile(),
			0,
			size_in_bits,
			0, // TODO: Alignment
			llvm::DINode::FlagTypePassByValue,
			nullptr,
			llvm::DINodeArray(), // We fill in later because we need to prevent circular dependency.
			0, // RunTimeLang. Clang ignores this
			nullptr,
			ll_struct_type->getName() // Unique name
		);
		di_cached_types.insert({ type, di_struct_type });

		auto di_ptr_member_type = builder.createMemberType(
			di_struct_type,
			"ptr",
			di_unit->getFile(),
			0,
			size_in_bits,
			0, // TODO: Alignment
			0, // bits offset
			llvm::DINode::DIFlags::FlagZero,
			emit_type(context.void_ptr_type)
		);
		auto di_length_member_type = builder.createMemberType(
			di_struct_type,
			"length",
			di_unit->getFile(),
			0,
			size_in_bits,
			0, // TODO: Alignment
			ll_struct_layout->getElementOffsetInBits(1), // bits offset
			llvm::DINode::DIFlags::FlagZero,
			emit_type(context.int_type)
		);

		builder.replaceArrays(di_struct_type, builder.getOrCreateArray({
			di_ptr_member_type,
			di_length_member_type
		}));

		return di_struct_type;
	}
	case TypeKind::Function: {
		unsigned ptr_size_in_bits = context.get_ll_module().getDataLayout().getPointerSizeInBits();

		llvm::SmallVector<llvm::Metadata*> di_func_types;
		auto func_type = static_cast<FunctionType*>(type);

		auto di_ret_type = emit_type(func_type->get_return_type());
		di_func_types.push_back(di_ret_type);

		for (Type* param_type : func_type->get_param_types()) {
			di_func_types.push_back(emit_type(param_type));
		}

		auto di_subroutine_type = builder.createSubroutineType(builder.getOrCreateTypeArray(di_func_types));
		auto di_func_type       = builder.createPointerType(di_subroutine_type, ptr_size_in_bits);

		di_cached_types.insert({ type, di_func_type });
		return di_func_type;
	}
	case TypeKind::Struct: {
		auto struct_type = static_cast<StructType*>(type);
		auto structn = struct_type->get_struct();
		auto file = structn->file;
		// Because types can be referenced before any codegen associated with it's file
		// as been generated we have to make sure the di_unit for that file is created.
		file->di_emitter->emit_file(file);

		auto ll_struct_type   = gen_struct_type(struct_type, context.get_ll_context(), context.get_ll_module());
		auto ll_struct_layout = context.get_ll_module().getDataLayout().getStructLayout(ll_struct_type);
		// This is right its underlying implementation gets the size of types by literally getting the size
		// of C++ types which they map to.
		uint64_t size_in_bits = ll_struct_layout->getSizeInBits();

		size_t line_number = file->line_table.get_line_number(structn->loc);

		llvm::DINode::DIFlags di_flags = llvm::DINode::FlagZero; // TODO: flags

		auto di_unit = file->di_emitter->di_unit;

		auto di_struct_type = builder.createStructType(
			nullptr, // TODO: Scope?
			structn->name.to_string(),
			di_unit->getFile(),
			static_cast<unsigned>(line_number),
			size_in_bits,
			0, // TODO: Alignment
			di_flags | llvm::DINode::FlagTypePassByValue,
			nullptr, // TODO: Derived from?
			llvm::DINodeArray(), // We fill in later because we need to prevent circular dependency.
			0, // RunTimeLang. Clang ignores this
			nullptr, // TODO: VTable holder?
			ll_struct_type->getName() // Unique name
		);
		di_cached_types.insert({ type, di_struct_type });

		auto create_member_field_type = [this, di_struct_type, file, di_unit](Var* field, uint64_t bits_offset) finline{
			auto ll_type = gen_type(field->type, context.get_ll_context(), context.get_ll_module());

			uint64_t size_in_bits = context.get_ll_module().getDataLayout().getTypeAllocSize(ll_type);

			size_t line_number = file->line_table.get_line_number(field->loc);

			return builder.createMemberType(
				di_struct_type,
				field->name.to_string(),
				di_unit->getFile(),
				static_cast<unsigned>(line_number),
				size_in_bits,
				0, // TODO: Alignment
				bits_offset,
				llvm::DINode::DIFlags::FlagZero,
				emit_type(field->type)
			);
		};

		llvm::SmallVector<llvm::Metadata*> di_field_types;
		for (Var* field : structn->fields) {
			uint64_t offset_in_bits = ll_struct_layout->getElementOffsetInBits(field->field_idx);
			di_field_types.push_back(create_member_field_type(field, offset_in_bits));
		}

		// TODO: Clang will add the functions as part of the type information. This
		// might be important?

		builder.replaceArrays(di_struct_type, builder.getOrCreateArray(di_field_types));

		return di_struct_type;
	}
	case TypeKind::Enum: {
		auto enum_type = static_cast<EnumType*>(type);
		return emit_type(enum_type->get_index_type());
	}
	default:
		acorn_fatal("unreachable");
		return nullptr;
	}
}
