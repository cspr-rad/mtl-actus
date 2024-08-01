import Lake
open Lake DSL

package «actus» where
  -- add package configuration options here

lean_lib «Actus» where
  -- add library configuration options here

lean_lib «Tests» where
  -- add library configuration options here

@[test_driver]
lean_exe «test» where
  root := `Tests.Main
