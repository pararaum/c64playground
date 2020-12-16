#include <stdbool.h>
#include <CUnit/Basic.h>
#include <CUnit/Console.h>
#include "xorshiftprng.h"

int init_suite_success(void) {
  return 0;
}
int init_suite_failure(void) {
  return -1;
}
int clean_suite_success(void) {
  return 0;
}
int clean_suite_failure(void) {
  return -1;
}

void test_success1(void) {
  CU_ASSERT(true);
}

void test_success2(void) {
  CU_ASSERT_NOT_EQUAL(2, -1);
}

void test_success3(void) {
  CU_ASSERT_STRING_EQUAL("string #1", "string #1");
}

void test_success4(void) {
  CU_ASSERT_STRING_NOT_EQUAL("string #1", "string #2");
}

void test_failure1(void) {
  CU_ASSERT(false);
}

void test_failure2(void) {
  CU_ASSERT_EQUAL(2, 3);
}

void test_failure3(void) {
  CU_ASSERT_STRING_NOT_EQUAL("string #1", "string #1");
}

void test_failure4(void) {
  CU_ASSERT_STRING_EQUAL("string #1", "string #2");
}

void test_xorshiftprng(void) {
  CU_ASSERT_EQUAL(xorshiftprng(), 0x2AD5);
  CU_ASSERT_EQUAL(xorshiftprng(), 0x3575);
  CU_ASSERT_EQUAL(xorshiftprng(), 0x3db2);
  CU_ASSERT_EQUAL(xorshiftprng(), 0x24c0);
}

int main() {
  CU_pSuite pSuite = NULL;

  /* initialize the CUnit test registry */
  if(CUE_SUCCESS != CU_initialize_registry()) {
    return CU_get_error();
  }

  /* add a suite to the registry */
  pSuite = CU_add_suite("Suite_PRNG", init_suite_success, clean_suite_success);
  if(NULL == pSuite) {
    CU_cleanup_registry();
    return CU_get_error();
  }

  /* add the tests to the suite */
  if((NULL == CU_add_test(pSuite, "test_xorshiftprng", test_xorshiftprng)) ||
     (NULL == CU_add_test(pSuite, "successful_test_2", test_success2))) {
    CU_cleanup_registry();
    return CU_get_error();
  }

  /* Run all tests using the basic interface */
  CU_basic_set_mode(CU_BRM_VERBOSE);
  CU_basic_run_tests();
  printf("\n");
  CU_basic_show_failures(CU_get_failure_list());
  printf("\n\n");

  /* Run all tests using the console interface */
  //CU_console_run_tests();

  /* Clean up registry and return */
  CU_cleanup_registry();
  return CU_get_error();
}
