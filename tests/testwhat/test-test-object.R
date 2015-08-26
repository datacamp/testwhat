source("testing-framework.R")
source("scen-test-object.R")

# CALL TESTS
test_call("test_object receives name argument", {
  expect_error(test_object(), "argument \"name\" is missing")
})

# SCENARIO TESTS
set_scenarios(scenarios)

test_scenario("handle equivalence correctly", "test_equivalence",  {
  expect_error(test_object("var.not_equiv"))
  test_object("var.equiv")
})

test_scenario("difference between equals and equivalent", "test_equal", {
  expect_error(test_object("df.equiv", eq_condition = "equal"))
  test_object("df.equiv")
  expect_error(test_object("df.not_equiv", eq_condition = "equal"))
  expect_error(test_object("df.not_equiv"))
})

test_scenario("difference between equals and identical", "test_identical", {
  expect_error(test_object("var.equal", eq_condition = "identical"))
  test_object("var.iden", eq_condition = "identical")
  test_object("var.equal")
})

test_scenario("test on eval parameter", "test_eval", {
  expect_error(test_object("var"))
  test_object("var", eval = FALSE)
  expect_error(test_object("var.not_here", eval=FALSE))
})

test_scenario("test on undefined message", "test_equivalence", {
  expect_error(test_object("var.not_here", undefined_msg="This is the undefined message"), "This is the undefined message")
  expect_error(expect_error(test_object("var.not_here", undefined_msg="This is the undefined message"), "This is not the undefined message"))
})

test_scenario("test on incorrect message", "test_equal", {
  expect_error(test_object("df.equiv", eq_condition = "equal", incorrect_msg = "This is the incorrect message"), "This is the incorrect message")
  expect_error(expect_error(test_object("df.equiv", eq_condition = "equal", incorrect_msg = "This is the incorrect message"), "This is not the incorrect message"))
})