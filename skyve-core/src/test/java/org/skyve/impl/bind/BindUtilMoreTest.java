package org.skyve.impl.bind;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

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
		assertFalse(BindUtil.containsSkyveExpressions("hello world"));
	}

	@Test
	void containsSkyveExpressionsWithOpenBraceAtStartReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("{binding}"));
	}

	@Test
	void containsSkyveExpressionsWithOpenBraceNotAtStartReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("hello {binding}"));
	}

	@Test
	void containsSkyveExpressionsWithEscapedBraceReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("hello \\{not an expression}"));
	}

	@Test
	void containsSkyveExpressionsEmptyStringReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions(""));
	}

	@Test
	void containsSkyveExpressionsOnlyEscapedBraceReturnsFalse() {
		assertFalse(BindUtil.containsSkyveExpressions("\\{escaped}"));
	}

	@Test
	void containsSkyveExpressionsMultipleExpressionsReturnsTrue() {
		assertTrue(BindUtil.containsSkyveExpressions("{first} and {second}"));
	}

	// --- isSkyveExpression ---

	@Test
	void isSkyveExpressionValidExpressionReturnsTrue() {
		assertTrue(BindUtil.isSkyveExpression("{binding}"));
	}

	@Test
	void isSkyveExpressionNoBracesReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("binding"));
	}

	@Test
	void isSkyveExpressionOnlyOpenBraceReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{binding"));
	}

	@Test
	void isSkyveExpressionOnlyCloseBraceReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("binding}"));
	}

	@Test
	void isSkyveExpressionSingleCharReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{"));
	}

	@Test
	void isSkyveExpressionEmptyBracesReturnsTrue() {
		// length > 1, starts with { and ends with } -> true even if empty content
		assertTrue(BindUtil.isSkyveExpression("{}"));
	}

	@Test
	void isSkyveExpressionLeadingTextReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("text{binding}"));
	}

	@Test
	void isSkyveExpressionTrailingTextReturnsFalse() {
		assertFalse(BindUtil.isSkyveExpression("{binding}text"));
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
