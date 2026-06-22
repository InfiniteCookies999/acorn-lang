## Acorn
General purpose programming language similar to C++ which is compiled using the LLVM framework. The language has support for  type deduction, reflection, modularity, and object lifetimes with destructors.


```
import std.io;

struct You {
    name: String;
    hobbies: const char*[3];
}

fn main() {
    you := You{
        name=String{"Maddie"},
        hobbies=["Programming", "Anime", "Coffee Drinking"]
    };
    io.println("Welcome to acorn %s!\n%s", you.name, you);
}

```

### Installation

LLVM: Version 20.1 installed

Windows: must have msvc linker installed to link programs.

Linux: must have clang installed to link programs.

To compile programs with the standard library you must have `acorn_std_lib` environment variable set to point to the `std` directory of the standard library.
