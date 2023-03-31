import Lake
open Lake DSL

package «efg» {
  -- add package configuration options here
}

lean_lib «Efg» {
  -- add library configuration options here
}

@[default_target]
lean_exe «efg» {
  root := `Main
}
