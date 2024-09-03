
add_executable (driver "Driver.cpp" )
set_target_properties (
    driver
    PROPERTIES
        OUTPUT_NAME "acorn"
)

target_link_libraries(driver LINK_PUBLIC acorn)
