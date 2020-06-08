with GNAT.Bind_Environment; use GNAT.Bind_Environment;

separate (Templates_Parser)
function Version return String is
begin
   return Get ("version");
end Version;