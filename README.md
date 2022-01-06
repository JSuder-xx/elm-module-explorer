# Overview

This is a VS Code extension for Elm that displays how the functions declared in the current file relate to types also declared in that file.

- In-Output: A function that returns the type.
- In-Input: A function that includes the type as an input.
- Input-And-Output: A function has the type in both input and output positions (potential updater).
- References: A function that simply includes the type. For example, (Widget -> Int) -> Int -> Bool does not really have Widget in an input position in that a Widget will not be provided to the function. Instead the function accepts another function which takes a Widget which means that internally the outer function will need to provide the Widget to the inner function.

## Todo

- Organize functions for types not declared in the current module.
