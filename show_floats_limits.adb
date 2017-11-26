with Ada.Text_IO;

procedure Show_floats_limits is

  generic
    type F is digits <>;
    name: String;
  procedure Show_limits;
  procedure Show_limits is
  begin
    Ada.Text_IO.Put_Line(name & F'Image(F'Last));
  end Show_limits;

  procedure SF is new Show_limits(Float,"Float");
  procedure SLF is new Show_limits(Long_Float,"Long_Float");
  procedure SLLF is new Show_limits(Long_Long_Float,"Long_Long_Float");

begin
  SF;
  SLF;
  SLLF;
end Show_floats_limits;
