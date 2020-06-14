module Language.JavaScript.Dependencies
  ( JSAST
  , Dependencies
  , jsAstDependencies
  ) where

import PacchettiBotti.Prelude

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Network.URI as URI
import qualified Turtle

import Data.Set (Set)
import Language.JavaScript.Parser.AST

type ModuleId = String
type Dependencies = Set Text

-- | Returns a set of optionaly scoped npm packages names for a module identifier.
dependency :: ModuleId -> Dependencies
dependency moduleId
-- Paths, relative and absolute, are valid module identifiers.
  | not . Turtle.absolute $ Turtle.decodeString moduleId
  , not $  "." `List.isPrefixOf` moduleId
  = Set.singleton $ Text.pack moduleId
  | otherwise
  = mempty

-- | A predicate for the [`require`](https://nodejs.org/api/modules.html#modules_require_id) CommonJS function.
isCommonJsRequire :: JSExpression -> Bool
isCommonJsRequire = \case
  JSMemberDot (JSIdentifier _ "module") _ (JSIdentifier _ "require") -> True
  JSCallExpressionDot (JSIdentifier _ "module") _ (JSIdentifier _ "require") -> True
  JSIdentifier _ "require" -> True
  _ -> False

-- | Collects all third party dependencies of a JavaScript AST.
jsAstDependencies :: JSAST -> Dependencies
jsAstDependencies = \case
  JSAstProgram statements _ ->
    foldMap jsStatementDependencies statements
  JSAstModule items _ ->
    foldMap jsModuleItemDependencies items
  JSAstStatement statement _ ->
    jsStatementDependencies statement
  JSAstExpression expression _ ->
    jsExpressionDependencies expression
  JSAstLiteral expression _ ->
    jsExpressionDependencies expression

jsModuleItemDependencies :: JSModuleItem -> Dependencies
jsModuleItemDependencies = \case
  JSModuleImportDeclaration _ declaration
    | JSImportDeclaration _ (JSFromClause _ _ moduleId) _ <- declaration
    , not $ URI.isURI moduleId
    -> dependency $ unescape moduleId
    | JSImportDeclarationBare _ moduleId _ <- declaration
    , not $ URI.isURI moduleId
    -> dependency $ unescape moduleId
    | otherwise -> mempty
  JSModuleExportDeclaration {} -> mempty
  JSModuleStatementListItem statement ->
    jsStatementDependencies statement

jsStatementDependencies :: JSStatement -> Dependencies
jsStatementDependencies = \case
  JSStatementBlock _ statements _ _ ->
    foldMap jsStatementDependencies statements
  JSBreak {} -> mempty
  JSLet _ declarations _ ->
    foldMapJSCommaList jsExpressionDependencies declarations
  JSClass _ _ super _ methods _ _ ->
       jsClassHeritageDependencies super
    <> foldMap jsClassElementDependencies methods
  JSConstant _ declarations _ ->
    foldMapJSCommaList jsExpressionDependencies declarations
  JSContinue {} -> mempty
  JSDoWhile _ body _ _ condition _ _ ->
       jsStatementDependencies body
    <> jsExpressionDependencies condition
  JSFor _ _ declarations _ condition _ update _ body ->
       jsForStatementDependencies declarations condition update body
  JSForIn _ _ declaration _ expression _ body ->
       jsForInStatementDependencies declaration expression body
  JSForVar _ _ _ declarations _ condition _ update _ body ->
       jsForStatementDependencies declarations condition update body
  JSForVarIn _ _ _ declaration _ expression _ body ->
       jsForInStatementDependencies declaration expression body
  JSForLet _ _ _ declarations _ condition _ update _ body ->
       jsForStatementDependencies declarations condition update body
  JSForLetIn _ _ _ declaration _ expression _ body ->
       jsForInStatementDependencies declaration expression body
  JSForLetOf _ _ _ declaration _ expression _ body ->
       jsForOfStatementDependencies declaration expression body
  JSForConst _ _ _ declarations _ condition _ update _ body ->
       jsForStatementDependencies declarations condition update body
  JSForConstIn _ _ _ declaration _ expression _ body ->
       jsForInStatementDependencies declaration expression body
  JSForConstOf _ _ _ declaration _ expression _ body ->
       jsForOfStatementDependencies declaration expression body
  JSForOf _ _ declaration _ expression _ body ->
       jsForOfStatementDependencies declaration expression body
  JSForVarOf _ _ _ declaration _ expression _ body ->
       jsForOfStatementDependencies declaration expression body
  JSFunction _ _ _ parameters _ body _ ->
       foldMapJSCommaList jsExpressionDependencies parameters
    <> jsBlockDependencies body
  JSGenerator _ _ _ _ parameters _ body _ ->
       foldMapJSCommaList jsExpressionDependencies parameters
    <> jsBlockDependencies body
  JSIf _ _ condition _ body ->
       jsExpressionDependencies condition
    <> jsStatementDependencies body
  JSIfElse _ _ condition _ consequent _ alternative ->
       jsExpressionDependencies condition
    <> jsStatementDependencies consequent
    <> jsStatementDependencies alternative
  JSLabelled _ _ statement ->
    jsStatementDependencies statement
  JSEmptyStatement {} -> mempty
  JSExpressionStatement expression _ ->
    jsExpressionDependencies expression
  JSAssignStatement assignee _ assigned _ ->
       jsExpressionDependencies assignee
    <> jsExpressionDependencies assigned
  JSMethodCall function _ arguments _ _
    | isCommonJsRequire function
    , JSLOne (JSStringLiteral _ moduleId) <- arguments
    -> dependency $ unescape moduleId
    | otherwise
    -> jsExpressionDependencies function
    <> foldMapJSCommaList jsExpressionDependencies arguments
  JSReturn _ optionalExpression _ ->
    foldMap jsExpressionDependencies optionalExpression
  JSSwitch _ _ scrutinee _ _ cases _ _ ->
       jsExpressionDependencies scrutinee
    <> foldMap jsSwitchPartsDependencies cases
    where
      jsSwitchPartsDependencies = \case
        JSCase _ pattern _ body ->
             jsExpressionDependencies pattern
          <> foldMap jsStatementDependencies body
        JSDefault _ _ body ->
          foldMap jsStatementDependencies body
  JSThrow _ exception _ ->
    jsExpressionDependencies exception
  JSTry _ block jsTryCatches jstTryFinally ->
       jsBlockDependencies block
    <> foldMap jsTryCatchDependencies jsTryCatches
    <> jsTryFinallyDependencies jstTryFinally
    where
      jsTryCatchDependencies = \case
        JSCatch _ _ exception _ handler ->
             jsExpressionDependencies exception
          <> jsBlockDependencies handler
        JSCatchIf {} -> mempty -- non standard
      jsTryFinallyDependencies = \case
        JSFinally _ handler -> jsBlockDependencies handler
        JSNoFinally -> mempty
  JSVariable _ declarations _ ->
    foldMapJSCommaList jsExpressionDependencies declarations
  JSWhile _ _ condition _ body ->
       jsExpressionDependencies condition
    <> jsStatementDependencies body
  JSWith _ _ scope _ body _ ->
       jsExpressionDependencies scope
    <> jsStatementDependencies body

foldMapJSCommaList :: Monoid m => (a -> m) -> JSCommaList a -> m
foldMapJSCommaList f = \case
  JSLCons xs _ x ->
    foldMapJSCommaList f xs <> f x
  JSLOne value -> f value
  JSLNil -> mempty

foldMapJSCommaTrailingList :: Monoid m => (a -> m) -> JSCommaTrailingList a -> m
foldMapJSCommaTrailingList f = \case
  JSCTLComma xs _ -> foldMapJSCommaList f xs
  JSCTLNone xs -> foldMapJSCommaList f xs

jsForStatementDependencies
  :: JSCommaList JSExpression
  -> JSCommaList JSExpression
  -> JSCommaList JSExpression
  -> JSStatement
  -> Dependencies
jsForStatementDependencies declarations condition update body =
     foldMapJSCommaList jsExpressionDependencies declarations
  <> foldMapJSCommaList jsExpressionDependencies condition
  <> foldMapJSCommaList jsExpressionDependencies update
  <> jsStatementDependencies body

jsForInStatementDependencies :: JSExpression -> JSExpression -> JSStatement -> Dependencies
jsForInStatementDependencies declaration expression body =
     jsExpressionDependencies declaration
  <> jsExpressionDependencies expression
  <> jsStatementDependencies body

jsForOfStatementDependencies :: JSExpression -> JSExpression -> JSStatement -> Dependencies
jsForOfStatementDependencies declaration expression body =
     jsExpressionDependencies declaration
  <> jsExpressionDependencies expression
  <> jsStatementDependencies body

jsBlockDependencies :: JSBlock -> Dependencies
jsBlockDependencies (JSBlock _ statements _) =
  foldMap jsStatementDependencies statements

jsExpressionDependencies :: JSExpression -> Dependencies
jsExpressionDependencies = \case
  JSIdentifier {} -> mempty
  JSDecimal {} -> mempty
  JSLiteral {} -> mempty
  JSHexInteger {} -> mempty
  JSOctal {} -> mempty
  JSStringLiteral {} -> mempty
  JSRegEx {} -> mempty
  JSArrayLiteral _ array _ -> flip foldMap array $ \case
    JSArrayElement expression ->
      jsExpressionDependencies expression
    JSArrayComma _ -> mempty
  JSAssignExpression assignee _ assigned ->
       jsExpressionDependencies assignee
    <> jsExpressionDependencies assigned
  JSCallExpression function _ arguments _
    | isCommonJsRequire function
    , JSLOne (JSStringLiteral _ moduleId) <- arguments
    -> dependency $ unescape moduleId
    | otherwise
    -> jsExpressionDependencies function
    <> foldMapJSCommaList jsExpressionDependencies arguments
  JSCallExpressionDot receiver _ property ->
       jsExpressionDependencies receiver
    <> jsExpressionDependencies property
  JSCallExpressionSquare receiver _ property _ ->
       jsExpressionDependencies receiver
    <> jsExpressionDependencies property
  JSClassExpression _ _ super _ methods _ ->
       jsClassHeritageDependencies super
    <> foldMap jsClassElementDependencies methods
  JSCommaExpression lhs _ rhs ->
       jsExpressionDependencies lhs
    <> jsExpressionDependencies rhs
  JSExpressionBinary lhs _ rhs ->
       jsExpressionDependencies lhs
    <> jsExpressionDependencies rhs
  JSExpressionParen _ expression _ ->
    jsExpressionDependencies expression
  JSExpressionPostfix expression _ ->
    jsExpressionDependencies expression
  JSExpressionTernary condition _ consequent _ alternative ->
       jsExpressionDependencies condition
    <> jsExpressionDependencies consequent
    <> jsExpressionDependencies alternative
  JSArrowExpression parameters _ body ->
       jsArrowParameterListDependencies parameters
    <> jsStatementDependencies body
    where
      jsArrowParameterListDependencies = \case
        JSUnparenthesizedArrowParameter {} -> mempty
        JSParenthesizedArrowParameterList _ parameters' _ ->
          foldMapJSCommaList jsExpressionDependencies parameters'
  JSFunctionExpression _ _ _ parameters _ body ->
       foldMapJSCommaList jsExpressionDependencies parameters
    <> jsBlockDependencies body
  JSGeneratorExpression _ _ _ _ parameters _ body ->
       foldMapJSCommaList jsExpressionDependencies parameters
    <> jsBlockDependencies body
  JSMemberDot receiver _ property ->
       jsExpressionDependencies receiver
    <> jsExpressionDependencies property
  JSMemberExpression function _ arguments _
    | isCommonJsRequire function
    , JSLOne (JSStringLiteral _ moduleId) <- arguments
    -> dependency $ unescape moduleId
    | otherwise
    -> jsExpressionDependencies function
    <> foldMapJSCommaList jsExpressionDependencies arguments
  JSMemberNew _ constructor _ arguments _ ->
       jsExpressionDependencies constructor
    <> foldMapJSCommaList jsExpressionDependencies arguments
  JSMemberSquare receiver _ property _ ->
       jsExpressionDependencies receiver
    <> jsExpressionDependencies property
  JSNewExpression _ constructor ->
    jsExpressionDependencies constructor
  JSObjectLiteral _ properties _ ->
    foldMapJSCommaTrailingList jsObjectPropertyDependencies properties
    where
      jsObjectPropertyDependencies = \case
        JSPropertyNameandValue name _ value ->
             jsPropertyNameDependencies name
          <> foldMap jsExpressionDependencies value
        JSPropertyIdentRef {} -> mempty
        JSObjectMethod method ->
          jsMethodDefinitionDependencies method
  JSSpreadExpression _ expression  ->
    jsExpressionDependencies expression
  JSTemplateLiteral optionalTag _ _ parts ->
       foldMap jsExpressionDependencies optionalTag
    <> foldMap jsTemplatePartDependencies parts
    where
      jsTemplatePartDependencies (JSTemplatePart expression _ _ ) =
        jsExpressionDependencies expression
  JSUnaryExpression _ expression ->
    jsExpressionDependencies expression
  JSVarInitExpression _ (JSVarInit _ initializer) ->
    jsExpressionDependencies initializer
  JSVarInitExpression _ JSVarInitNone -> mempty
  JSYieldExpression _ optionalExpression ->
    foldMap jsExpressionDependencies optionalExpression
  JSYieldFromExpression _ _ expression ->
    jsExpressionDependencies expression

jsClassHeritageDependencies :: JSClassHeritage -> Dependencies
jsClassHeritageDependencies = \case
  JSExtends _ super -> jsExpressionDependencies super
  JSExtendsNone -> mempty

jsClassElementDependencies :: JSClassElement -> Dependencies
jsClassElementDependencies = \case
  JSClassInstanceMethod method ->
    jsMethodDefinitionDependencies method
  JSClassStaticMethod _ method ->
    jsMethodDefinitionDependencies method
  JSClassSemi _ -> mempty

jsMethodDefinitionDependencies :: JSMethodDefinition -> Dependencies
jsMethodDefinitionDependencies = \case
  JSMethodDefinition name _ parameters _ body ->
       jsPropertyNameDependencies name
    <> foldMapJSCommaList jsExpressionDependencies parameters
    <> jsBlockDependencies body
  JSGeneratorMethodDefinition _ name _ parameters _ body ->
       jsPropertyNameDependencies name
    <> foldMapJSCommaList jsExpressionDependencies parameters
    <> jsBlockDependencies body
  JSPropertyAccessor _ name _ parameters _ body ->
       jsPropertyNameDependencies name
    <> foldMapJSCommaList jsExpressionDependencies parameters
    <> jsBlockDependencies body

jsPropertyNameDependencies :: JSPropertyName -> Dependencies
jsPropertyNameDependencies = \case
  JSPropertyIdent {} -> mempty
  JSPropertyString {} -> mempty
  JSPropertyNumber {} -> mempty
  JSPropertyComputed _ expression _ ->
    jsExpressionDependencies expression

unescape :: String -> String
unescape str = go $ List.drop 1 str
  where
  go ('\\' : 'b' : xs) = '\b' : go xs
  go ('\\' : 'f' : xs) = '\f' : go xs
  go ('\\' : 'n' : xs) = '\n' : go xs
  go ('\\' : 'r' : xs) = '\r' : go xs
  go ('\\' : 't' : xs) = '\t' : go xs
  go ('\\' : 'v' : xs) = '\v' : go xs
  go ('\\' : '0' : xs) = '\0' : go xs
  go ('\\' : 'x' : a : b : xs) = Char.chr (a' + b') : go xs
    where
    a' = 16 * Char.digitToInt a
    b' = Char.digitToInt b
  go ('\\' : 'u' : a : b : c : d : xs) = Char.chr (a' + b' + c' + d') : go xs
    where
    a' = 16 * 16 * 16 * Char.digitToInt a
    b' = 16 * 16 * Char.digitToInt b
    c' = 16 * Char.digitToInt c
    d' = Char.digitToInt d
  go ('\\' : x : xs) = x : go xs
  go "\"" = ""
  go "'" = ""
  go (x : xs) = x : go xs
  go "" = ""
