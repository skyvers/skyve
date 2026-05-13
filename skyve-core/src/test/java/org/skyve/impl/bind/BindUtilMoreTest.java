package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;

@SuppressWarnings("static-method")
class BindUtilMoreTest {

	// --- containsSkyveExpressions ---

	@Test
	void containsSkyveExpressionsNoBracesReturnsFalse() {
		assertThat(BindUtil.containsSkyveExpressions("hello world"), is(false));
	}

	@Test
	void containsSkyveExpressionsWithOpenBraceAtStartReturnsTrue() {
		assertThat(BindUtil.containsSkyveExpressions("{binding}"), is(true));
	}

	@Test
	void containsSkyveExpressionsWithOpenBraceNotAtStartReturnsTrue() {
		assertThat(BindUtil.containsSkyveExpressions("hello {binding}"), is(true));
	}

	@Test
	void containsSkyveExpressionsWithEscapedBraceReturnsFalse() {
		assertThat(BindUtil.containsSkyveExpressions("hello \\{not an expression}"), is(false));
	}

	@Test
	void containsSkyveExpressionsEmptyStringReturnsFalse() {
		assertThat(BindUtil.containsSkyveExpressions(""), is(false));
	}

	@Test
	void containsSkyveExpressionsOnlyEscapedBraceReturnsFalse() {
		assertThat(BindUtil.containsSkyveExpressions("\\{escaped}"), is(false));
	}

	@Test
	void containsSkyveExpressionsMultipleExpressionsReturnsTrue() {
		assertThat(BindUtil.containsSkyveExpressions("{first} and {second}"), is(true));
	}

	// --- isSkyveExpression ---

	@Test
	void isSkyveExpressionValidExpressionReturnsTrue() {
		assertThat(BindUtil.isSkyveExpression("{binding}"), is(true));
	}

	@Test
	void isSkyveExpressionNoBracesReturnsFalse() {
		assertThat(BindUtil.isSkyveExpression("binding"), is(false));
	}

	@Test
	void isSkyveExpressionOnlyOpenBraceReturnsFalse() {
		assertThat(BindUtil.isSkyveExpression("{binding"), is(false));
	}

	@Test
	void isSkyveExpressionOnlyCloseBraceReturnsFalse() {
		assertThat(BindUtil.isSkyveExpression("binding}"), is(false));
	}

	@Test
	void isSkyveExpressionSingleCharReturnsFalse() {
		assertThat(BindUtil.isSkyveExpression("{"), is(false));
	}

	@Test
	void isSkyveExpressionEmptyBracesReturnsTrue() {
		// length > 1, starts with { and ends with } -> true even if empty content
		assertThat(BindUtil.isSkyveExpression("{}"), is(true));
	}

	@Test
	void isSkyveExpressionLeadingTextReturnsFalse() {
		assertThat(BindUtil.isSkyveExpression("text{binding}"), is(false));
	}

	@Test
	void isSkyveExpressionTrailingTextReturnsFalse() {
		assertThat(BindUtil.isSkyveExpression("{binding}text"), is(false));
	}

	// --- implicitAttributeType ---

	@Test
	void implicitAttributeTypeBizIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DOCUMENT_ID));
	}

	@Test
	void implicitAttributeTypeBizKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.BIZ_KEY));
	}

	@Test
	void implicitAttributeTypeCustomerNameReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.CUSTOMER_NAME));
	}

	@Test
	void implicitAttributeTypeDataGroupIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DATA_GROUP_ID));
	}

	@Test
	void implicitAttributeTypeUserIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.USER_ID));
	}

	@Test
	void implicitAttributeTypeModuleKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.MODULE_KEY));
	}

	@Test
	void implicitAttributeTypeDocumentKeyReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(Bean.DOCUMENT_KEY));
	}

	@Test
	void implicitAttributeTypeFlagCommentReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(PersistentBean.FLAG_COMMENT_NAME));
	}

	@Test
	void implicitAttributeTypeHierarchicalParentIdReturnsString() {
		assertEquals(String.class, BindUtil.implicitAttributeType(HierarchicalBean.PARENT_ID));
	}

	@Test
	void implicitAttributeTypeParentNameReturnsBean() {
		assertEquals(Bean.class, BindUtil.implicitAttributeType(ChildBean.PARENT_NAME));
	}

	@Test
	void implicitAttributeTypeOrdinalReturnsInteger() {
		assertEquals(Integer.class, BindUtil.implicitAttributeType(Bean.ORDINAL_NAME));
	}

	@Test
	void implicitAttributeTypeVersionReturnsInteger() {
		assertEquals(Integer.class, BindUtil.implicitAttributeType(PersistentBean.VERSION_NAME));
	}

	@Test
	void implicitAttributeTypeCreatedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.CREATED_KEY));
	}

	@Test
	void implicitAttributeTypeNotCreatedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_CREATED_KEY));
	}

	@Test
	void implicitAttributeTypePersistedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.PERSISTED_KEY));
	}

	@Test
	void implicitAttributeTypeNotPersistedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_PERSISTED_KEY));
	}

	@Test
	void implicitAttributeTypeChangedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.CHANGED_KEY));
	}

	@Test
	void implicitAttributeTypeNotChangedKeyReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(Bean.NOT_CHANGED_KEY));
	}

	@Test
	void implicitAttributeTypeTaggedReturnsBoolean() {
		assertEquals(Boolean.class, BindUtil.implicitAttributeType(PersistentBean.TAGGED_NAME));
	}

	@Test
	void implicitAttributeTypeLockReturnsOptimisticLock() {
		assertEquals(OptimisticLock.class, BindUtil.implicitAttributeType(PersistentBean.LOCK_NAME));
	}

	@Test
	void implicitAttributeTypeUnknownAttributeReturnsNull() {
		assertThat(BindUtil.implicitAttributeType("myCustomField"), is(nullValue()));
	}
}
