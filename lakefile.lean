import Lake
open Lake DSL

package «actus» where
  -- add package configuration options here

lean_lib «Actus» where
  -- add library configuration options here

@[default_target]
lean_exe «actus» where
  root := `Main
