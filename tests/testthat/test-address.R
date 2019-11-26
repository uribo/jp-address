test_that("address separate works", {
  expect_equal(
    separate_address("北海道札幌市中央区北1条西2丁目"),
    list(
      prefecture = "北海道",
      city = "札幌市中央区",
      street = "北1条西2丁目"
    )
  )
  expect_equal(
    separate_address("奈良県高市郡高取町"),
    list(
      prefecture = "奈良県",
      city = "高市郡高取町",
      street = NA_character_
    )
  )
  expect_equal(
    separate_address("北海道余市郡余市町朝日町"),
    list(
      prefecture = "北海道",
      city = "余市郡余市町",
      street = "朝日町"
    )
  )
  expect_equal(
    separate_address("北海道余市郡余市町黒川町十九丁目"),
    list(
      prefecture = "北海道",
      city = "余市郡余市町",
      street = "黒川町十九丁目"
    )
  )
  
})
