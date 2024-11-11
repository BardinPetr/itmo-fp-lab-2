let () =
  Utils.run_with_save_report "OABag" "Open addressing hash table bag"
    (Sys.getenv_opt "REPORT_PATH")
    (Unit.UnitTests.tests @ Prop.PropTests.tests @ Stringtest.StringTests.tests
   @ Maptest.tests)
