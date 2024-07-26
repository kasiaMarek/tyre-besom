package regex
import net.marek.tyre.*

case class ResourceType(pkg: String, modules: List[String], name: String)
case class URN(
    stack: String,
    project: String,
    parentTypes: List[ResourceType],
    resourceType: ResourceType,
    resourceName: String
)

/* any sequence of unicode code points that does not contain "::" */
val string: Tyre[String] = tyre"((([^:].)*.?)|(.?(.[^:])*))!s"
/* any sequence of unicode code points */
val anystring: Tyre[String] = tyre"(.*)!s"
val identifier: Tyre[String] = tyre"[a-zA-Z1-9_/.\-]*!s"

/* package ":" [ module ":" ] type name */
val tpe: Tyre[ResourceType] =
  tyre"$identifier:($identifier:)*$identifier".map {
    case (pkg, ':', modules, typeName) =>
      ResourceType(pkg, modules.map(_._1), typeName)
  }

/* [ parent type "$" ] type */
val qualifiedTypeName: Tyre[(List[ResourceType], ResourceType)] =
  tyre"($tpe$$)*".map(_.map(_._1)) <*> tpe

val urn: Tyre[URN] =
  tyre"(urn:pulumi:)$string(::)$string(::)$qualifiedTypeName(::)$anystring"
    .map {
      case (
            _,
            stack,
            _,
            project,
            _,
            (parentTypes, resourceType),
            _,
            name
          ) =>
        URN(stack, project, parentTypes, resourceType, name)
    }
val urnCompiled = urn.compile()
