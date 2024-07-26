import munit.BaseFunSuite

class ParseTests extends BaseFunSuite {

  val exampleNestedResource =
    URN(
      "urn:pulumi:stack::project::some:happy:component$custom:resources:Resource$besom:testing/test:Resource::my-test-resource"
    )

  val shortResourceTypeUrn =
    URN(
      "urn:pulumi:stack::project::some:happy:component$custom:Resource$besom:Resource::my-test-resource"
    )

  test("example-resource") {
    val expected =
      regex.URN(
        "stack",
        "project",
        List(
          regex.ResourceType("some", List("happy"), "component"),
          regex.ResourceType("custom", List("resources"), "Resource")
        ),
        regex.ResourceType("besom", List("testing/test"), "Resource"),
        "my-test-resource"
      )
    assertEquals(exampleNestedResource, expected)
  }

  test("URN.apply should only work for correct URNs") {
    val errs = compileErrors("URN(\"well::it's::not::a::urn\")")
    assertNoDiff(
      errs,
      """|error: This string doesn't match the URN format, see https://www.pulumi.com/docs/concepts/resources/names/#urns
         |    val errs = compileErrors("URN(\"well::it's::not::a::urn\")")
         |                           ^
         |""".stripMargin
    )
  }

}
