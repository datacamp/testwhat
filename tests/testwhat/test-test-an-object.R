source("testing-framework.R")
source("scen-test-an-object.R")

# CALL TESTS
test_call("test_an_object receives name argument", {
  expect_error(test_an_object(), "argument \"undefined_msg\" is missing")
})

# SCENARIO TESTS
set_scenarios(scenarios)

test_scenario("handle equivalence correctly", "test_equivalence",  {
  expect_error(test_an_object("var.not_equiv", undefined_msg = ""))
  test_an_object("var.equiv", undefined_msg = "")
})

test_scenario("difference between equals and equivalent", "test_equal", {
  expect_error(test_an_object("df.equiv", eq_condition = "equal", undefined_msg = ""))
  test_an_object("df.equiv", undefined_msg = "")
  expect_error(test_an_object("df.not_equiv", eq_condition = "equal", undefined_msg = ""))
  expect_error(test_an_object("df.not_equiv", undefined_msg = ""))
})

test_scenario("difference between equals and identical", "test_identical", {
  expect_error(test_an_object("var.equal", eq_condition = "identical", undefined_msg = ""))
  test_an_object("var.iden", eq_condition = "identical", undefined_msg = "")
  test_an_object("var.equal", undefined_msg = "")
})

test_scenario("test on undefined message", "test_equivalence", {
  expect_error(test_an_object("var.not_here", undefined_msg="This is the undefined message"), "This is the undefined message")
  expect_error(expect_error(test_an_object("var.not_here", undefined_msg="This is the undefined message"), "This is not the undefined message"))
})