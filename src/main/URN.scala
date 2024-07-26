import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.ToExpr
import scala.quoted.quotes
import compiletime.error

object URN:
  inline def apply(inline arg: String) =
    inline urn(arg) match
      case Some(v) => v
      case None => error("This string doesn't match the URN format, see https://www.pulumi.com/docs/concepts/resources/names/#urns")
    
  transparent inline def urn(inline str: String) = ${ urnImpl('{ str }) }

  private def urnImpl(str: Expr[String])(using Quotes) =
    import quotes.reflect.*
    Expr(regex.urnCompiled.run(str.valueOrAbort))

given ToExpr[regex.URN] with
  def apply(urn: regex.URN)(using Quotes): Expr[regex.URN] = 
    '{ regex.URN(${ Expr(urn.stack) }, ${ Expr(urn.project) }, ${ Expr(urn.parentTypes) }, ${ Expr(urn.resourceType)}, ${ Expr(urn.resourceName) } )}
  
given ToExpr[regex.ResourceType] with
  def apply(tpe: regex.ResourceType)(using Quotes): Expr[regex.ResourceType] = 
    '{ regex.ResourceType(${ Expr(tpe.pkg) }, ${ Expr(tpe.modules) }, ${ Expr(tpe.name) }) }
