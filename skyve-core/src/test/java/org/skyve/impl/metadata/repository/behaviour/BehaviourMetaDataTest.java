package org.skyve.impl.metadata.repository.behaviour;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.behaviour.IfStatement;
import org.skyve.impl.metadata.behaviour.InvokeStatement;
import org.skyve.impl.metadata.behaviour.InvokeStaticStatement;
import org.skyve.impl.metadata.behaviour.MethodArgument;
import org.skyve.impl.metadata.behaviour.SetStatement;
import org.skyve.impl.metadata.repository.behaviour.statement.StatementMetaData;
import org.skyve.metadata.MetaDataException;

@SuppressWarnings("static-method")
class BehaviourMetaDataTest {

	// ---- BizletMetaData ----

	@Test
	void bizletDefaultConstructorCreatesInstance() {
		BizletMetaData b = new BizletMetaData();
		assertNotNull(b);
	}

	@Test
	void bizletDocumentationRoundTrip() {
		BizletMetaData b = new BizletMetaData();
		b.setDocumentation("Some docs");
		assertEquals("Some docs", b.getDocumentation());
	}

	@Test
	void bizletDocumentationBlankBecomesNull() {
		BizletMetaData b = new BizletMetaData();
		b.setDocumentation("   ");
		assertNull(b.getDocumentation());
	}

	@Test
	void bizletLastModifiedMillisDefaultIsMaxValue() {
		BizletMetaData b = new BizletMetaData();
		assertEquals(Long.MAX_VALUE, b.getLastModifiedMillis());
	}

	@Test
	void bizletLastModifiedMillisRoundTrip() {
		BizletMetaData b = new BizletMetaData();
		b.setLastModifiedMillis(100L);
		assertEquals(100L, b.getLastModifiedMillis());
	}

	@Test
	void bizletLastCheckedMillisRoundTrip() {
		BizletMetaData b = new BizletMetaData();
		b.setLastCheckedMillis(200L);
		assertEquals(200L, b.getLastCheckedMillis());
	}

	@Test
	void bizletConvertReturnsSelf() {
		BizletMetaData b = new BizletMetaData();
		assertSame(b, b.convert("test"));
	}

	@Test
	void bizletPropertiesMapNotNull() {
		assertNotNull(new BizletMetaData().getProperties());
	}

	// ---- ActionMetaData ----

	@Test
	void actionDefaultConstructorCreatesInstance() {
		ActionMetaData a = new ActionMetaData();
		assertNotNull(a);
	}

	@Test
	void actionNameRoundTrip() {
		ActionMetaData a = new ActionMetaData();
		a.setName("myAction");
		assertEquals("myAction", a.getName());
	}

	@Test
	void actionNameBlankBecomesNull() {
		ActionMetaData a = new ActionMetaData();
		a.setName("  ");
		assertNull(a.getName());
	}

	@Test
	void actionDocumentationRoundTrip() {
		ActionMetaData a = new ActionMetaData();
		a.setDocumentation("Action docs");
		assertEquals("Action docs", a.getDocumentation());
	}

	@Test
	void actionStatementsListNotNull() {
		assertNotNull(new ActionMetaData().getStatements());
		assertTrue(new ActionMetaData().getStatements().isEmpty());
	}

	@Test
	void actionLastModifiedMillisDefaultIsMaxValue() {
		assertEquals(Long.MAX_VALUE, new ActionMetaData().getLastModifiedMillis());
	}

	@Test
	void actionLastModifiedMillisRoundTrip() {
		ActionMetaData a = new ActionMetaData();
		a.setLastModifiedMillis(99L);
		assertEquals(99L, a.getLastModifiedMillis());
	}

	@Test
	void actionLastCheckedMillisRoundTrip() {
		ActionMetaData a = new ActionMetaData();
		a.setLastCheckedMillis(88L);
		assertEquals(88L, a.getLastCheckedMillis());
	}

	@Test
	void actionConvertWithNameReturnsSelf() {
		ActionMetaData a = new ActionMetaData();
		a.setName("myAction");
		assertSame(a, a.convert("test"));
	}

	@Test
	void actionConvertWithNullNameThrows() {
		ActionMetaData a = new ActionMetaData();
		assertThrows(MetaDataException.class, () -> a.convert("test"));
	}

	@Test
	void actionPropertiesMapNotNull() {
		assertNotNull(new ActionMetaData().getProperties());
	}

	// ---- BizletMetaData lifecycle no-op methods ----

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletNewInstanceDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().newInstance(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletValidateDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().validate(null, null));
	}

	@Test
	void bizletGetConstantDomainValuesReturnsNull() throws Exception {
		assertNull(new BizletMetaData().getConstantDomainValues("attr"));
	}

	@Test
	void bizletGetVariantDomainValuesReturnsNull() throws Exception {
		assertNull(new BizletMetaData().getVariantDomainValues("attr"));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletGetDynamicDomainValuesReturnsNull() throws Exception {
		assertNull(new BizletMetaData().getDynamicDomainValues("attr", null));
	}

	@Test
	void bizletCompleteReturnsNull() throws Exception {
		assertNull(new BizletMetaData().complete("attr", "val", null));
	}

	@Test
	void bizletResolveReturnsNull() throws Exception {
		assertNull(new BizletMetaData().resolve("id", null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletPreSaveDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().preSave(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletPostSaveDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().postSave(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletPreDeleteDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().preDelete(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletPostDeleteDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().postDelete(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletPostLoadDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().postLoad(null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletPreRerenderDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().preRerender(null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void bizletPostRenderDoesNotThrow() {
		assertDoesNotThrow(() -> new BizletMetaData().postRender(null));
	}

	// ---- ActionMetaData.execute ----

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void actionExecuteWithEmptyStatementsDoesNotThrow() {
		ActionMetaData a = new ActionMetaData();
		assertDoesNotThrow(() -> a.execute(null));
	}

	// ---- ActionMetaData.convert() statement validation ----

	private static IfStatement ifStatement(String condition) {
		IfStatement result = new IfStatement();
		result.setCondition(condition);
		return result;
	}

	private static SetStatement setStatement(String binding, String expression) {
		SetStatement result = new SetStatement();
		result.setBinding(binding);
		result.setExpression(expression);
		return result;
	}

	private static MethodArgument argument(String typeName, String expression) {
		MethodArgument result = new MethodArgument();
		result.setTypeName(typeName);
		result.setExpression(expression);
		return result;
	}

	private static InvokeStaticStatement invokeStatic(String className, String methodName) {
		InvokeStaticStatement result = new InvokeStaticStatement();
		result.setClassName(className);
		result.setMethodName(methodName);
		return result;
	}

	private static ActionMetaData action(StatementMetaData... statements) {
		ActionMetaData result = new ActionMetaData();
		result.setName("myAction");
		for (StatementMetaData statement : statements) {
			result.getStatements().add(statement);
		}
		return result;
	}

	private static void assertConvertFails(ActionMetaData action, String expectedMessagePart) {
		MetaDataException e = assertThrows(MetaDataException.class, () -> action.convert("test"));
		assertTrue(e.getMessage().contains(expectedMessagePart), e.getMessage());
	}

	@Test
	void actionConvertValidatesNestedStatements() {
		IfStatement ifStatement = ifStatement("true");
		ifStatement.getThenStatements().add(setStatement("name", "'then'"));
		InvokeStaticStatement invoke = invokeStatic("java.lang.String", "valueOf");
		invoke.getArguments().add(argument("java.lang.Object", "{name}"));
		ifStatement.getElseStatements().add(invoke);
		InvokeStatement invokeMethod = new InvokeStatement();
		invokeMethod.setMethodName("toString");
		ActionMetaData a = action(ifStatement, invokeMethod);
		assertSame(a, a.convert("test"));
	}

	@Test
	void actionConvertThrowsWhenIfConditionIsNull() {
		assertConvertFails(action(ifStatement(null)), "The if [condition] is required for action myAction");
	}

	@Test
	void actionConvertThrowsWhenNestedSetBindingIsNull() {
		IfStatement ifStatement = ifStatement("true");
		ifStatement.getElseStatements().add(setStatement(null, "'x'"));
		assertConvertFails(action(ifStatement), "The set [binding] is required for action myAction");
	}

	@Test
	void actionConvertThrowsWhenSetExpressionIsNull() {
		assertConvertFails(action(setStatement("name", null)), "The set [expression] is required for action myAction");
	}

	@Test
	void actionConvertThrowsWhenInvokeStaticClassNameIsNull() {
		assertConvertFails(action(invokeStatic(null, "valueOf")), "The invokeStatic [className] is required for action myAction");
	}

	@Test
	void actionConvertThrowsWhenInvokeMethodNameIsNull() {
		assertConvertFails(action(invokeStatic("java.lang.String", null)), "The invoke [method] is required for action myAction");
	}

	@Test
	void actionConvertThrowsWhenArgumentTypeNameIsNull() {
		InvokeStaticStatement invoke = invokeStatic("java.lang.String", "valueOf");
		invoke.getArguments().add(argument(null, "{name}"));
		assertConvertFails(action(invoke), "The argument [typeName] is required for action myAction");
	}

	@Test
	void actionConvertThrowsWhenArgumentExpressionIsNull() {
		InvokeStaticStatement invoke = invokeStatic("java.lang.String", "valueOf");
		invoke.getArguments().add(argument("java.lang.Object", null));
		assertConvertFails(action(invoke), "The argument [expression] is required for action myAction");
	}
}
