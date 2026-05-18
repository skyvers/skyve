package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.ChildBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Association;
import org.skyve.metadata.model.document.Collection;
import org.skyve.metadata.model.document.Condition;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;

@SuppressWarnings({ "boxing", "static-method" })
class MetaDataExpressionEvaluatorTest {
	private static final class TestEvaluator extends MetaDataExpressionEvaluator {
		@Override
		public String formatWithoutPrefixOrSuffix(String expression, Bean bean) {
			return expression;
		}

		@Override
		public Object evaluateWithoutPrefixOrSuffix(String expression, Bean bean) {
			return expression;
		}

		@Override
		public void prefixBindingWithoutPrefixOrSuffix(StringBuilder expression, String binding) {
			expression.insert(0, binding + '.');
		}
	}

	@Test
	void validateWithoutPrefixOrSuffixRequiresNonNullContext() {
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);

		assertThrows(IllegalArgumentException.class,
				() -> evaluator.validateWithoutPrefixOrSuffix("a", null, null, module, document));
		assertThrows(IllegalArgumentException.class,
				() -> evaluator.validateWithoutPrefixOrSuffix("a", null, customer, null, document));
		assertThrows(IllegalArgumentException.class,
				() -> evaluator.validateWithoutPrefixOrSuffix("a", null, customer, module, null));
	}

	@Test
	void completeWithoutPrefixOrSuffixHandlesNullAndDelimiterOnlyFragments() {
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		doReturn(List.of()).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of());
		when(document.getParentDocumentName()).thenReturn(null);
		doReturn(false).when(document).isPersistable();

		List<String> nullResult = evaluator.completeWithoutPrefixOrSuffix(null, customer, mock(Module.class), document);
		assertNotNull(nullResult);

		List<String> dotResult = evaluator.completeWithoutPrefixOrSuffix(".", customer, mock(Module.class), document);
		assertNotNull(dotResult);
		assertTrue(dotResult.isEmpty());

		List<String> bracketResult = evaluator.completeWithoutPrefixOrSuffix("[", customer, mock(Module.class), document);
		assertNotNull(bracketResult);
		assertTrue(bracketResult.isEmpty());
	}

	@Test
	void addAttributesAndConditionsAddsExpectedImplicitAndConditionEntries() {
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Attribute alpha = mock(Attribute.class);
		when(alpha.getName()).thenReturn("alpha");
		Attribute beta = mock(Attribute.class);
		when(beta.getName()).thenReturn("beta");
		doReturn(List.of(alpha, beta)).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of("cond", Bean.CREATED_KEY, Bean.PERSISTED_KEY));
		when(document.getParentDocumentName()).thenReturn("ParentDoc");
		when(document.getName()).thenReturn("ChildDoc");
		doReturn(true).when(document).isPersistable();

		List<String> completions = new ArrayList<>();
		MetaDataExpressionEvaluator.addAttributesAndConditions("p.", null, customer, document, completions);

		assertTrue(completions.contains("p.alpha"));
		assertTrue(completions.contains("p.beta"));
		assertTrue(completions.contains("p.cond"));
		assertTrue(completions.contains("p.notCond"));
		assertFalse(completions.contains("p." + Bean.CREATED_KEY));
		assertTrue(completions.contains("p." + Bean.PERSISTED_KEY));
		assertTrue(completions.contains("p." + Bean.DOCUMENT_ID));
		assertTrue(completions.contains("p." + Bean.BIZ_KEY));
		assertTrue(completions.contains("p." + Bean.CHANGED_KEY));
		assertTrue(completions.contains("p." + Bean.NOT_CHANGED_KEY));
		assertTrue(completions.contains("p." + Bean.NOT_PERSISTED_KEY));
		assertTrue(completions.contains("p." + Bean.CUSTOMER_NAME));
		assertTrue(completions.contains("p." + Bean.DATA_GROUP_ID));
		assertTrue(completions.contains("p." + Bean.USER_ID));
		assertTrue(completions.contains("p." + ChildBean.PARENT_NAME));
		assertTrue(completions.contains("p." + PersistentBean.VERSION_NAME));
		assertTrue(completions.contains("p." + PersistentBean.LOCK_NAME));
		assertTrue(completions.contains("p." + PersistentBean.TAGGED_NAME));
		assertTrue(completions.contains("p." + PersistentBean.FLAG_COMMENT_NAME));
	}

	@Test
	void addAttributesAndConditionsRespectsFragmentFilteringAndHierarchicalParent() {
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Attribute alpha = mock(Attribute.class);
		when(alpha.getName()).thenReturn("alpha");
		Attribute gamma = mock(Attribute.class);
		when(gamma.getName()).thenReturn("gamma");
		doReturn(List.of(alpha, gamma)).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of("good", "other"));
		when(document.getParentDocumentName()).thenReturn("Node");
		when(document.getName()).thenReturn("Node");
		doReturn(false).when(document).isPersistable();

		List<String> completions = new ArrayList<>();
		MetaDataExpressionEvaluator.addAttributesAndConditions("", "g", customer, document, completions);

		assertTrue(completions.contains("gamma"));
		assertFalse(completions.contains("alpha"));
		assertTrue(completions.contains("good"));
		assertTrue(completions.contains(BindUtil.negateCondition("good")));
		assertFalse(completions.contains("other"));
		assertFalse(completions.contains(HierarchicalBean.PARENT_ID));
		assertFalse(completions.contains(PersistentBean.VERSION_NAME));
	}

	@Test
	void prefixBindingImplementationWorksInTestSubclass() {
		TestEvaluator evaluator = new TestEvaluator();
		StringBuilder value = new StringBuilder("name");
		evaluator.prefixBindingWithoutPrefixOrSuffix(value, "bean");
		assertEquals("bean.name", value.toString());
	}

	@Test
	void addAttributesAndConditionsIncludesHierarchicalParentIdWithNullFragment() {
		// When parent document name equals document name → hierarchical path
		// With null fragment → PARENT_ID should be added
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		doReturn(List.of()).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of());
		when(document.getParentDocumentName()).thenReturn("TreeNode");
		when(document.getName()).thenReturn("TreeNode");
		doReturn(false).when(document).isPersistable();

		List<String> completions = new ArrayList<>();
		MetaDataExpressionEvaluator.addAttributesAndConditions("", null, customer, document, completions);

		assertTrue(completions.contains(HierarchicalBean.PARENT_ID));
	}

	@Test
	void addAttributesAndConditionsIncludesPersistentFieldsWhenPersistable() {
		// isPersistable=true → VERSION_NAME, LOCK_NAME, TAGGED_NAME, FLAG_COMMENT_NAME added
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		doReturn(List.of()).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of());
		when(document.getParentDocumentName()).thenReturn(null);
		doReturn(true).when(document).isPersistable();

		List<String> completions = new ArrayList<>();
		MetaDataExpressionEvaluator.addAttributesAndConditions("", null, customer, document, completions);

		assertTrue(completions.contains(PersistentBean.VERSION_NAME));
		assertTrue(completions.contains(PersistentBean.LOCK_NAME));
		assertTrue(completions.contains(PersistentBean.TAGGED_NAME));
		assertTrue(completions.contains(PersistentBean.FLAG_COMMENT_NAME));
	}

	@Test
	void completeWithSimpleFragmentFiltersImplicitBindings() {
		// Non-compound fragment (no '.', '[', ']') takes the simple path
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		doReturn(List.of()).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of());
		when(document.getParentDocumentName()).thenReturn(null);
		doReturn(false).when(document).isPersistable();

		// "bi" prefix → matches bizId, bizKey (both start with "bi")
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("bi", customer, module, document);

		assertNotNull(result);
		assertTrue(result.contains(Bean.DOCUMENT_ID));     // "bizId"
		assertTrue(result.contains(Bean.BIZ_KEY));         // "bizKey"
		assertFalse(result.contains(Bean.CHANGED_KEY));  // "changed" — does not start with "bi"
	}

	@Test
	void completeWithSimpleFragmentMatchingAttributeName() {
		// Non-compound fragment matching an attribute name
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute nameAttr = mock(Attribute.class);
		when(nameAttr.getName()).thenReturn("firstName");
		Attribute otherAttr = mock(Attribute.class);
		when(otherAttr.getName()).thenReturn("lastName");
		doReturn(List.of(nameAttr, otherAttr)).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of());
		when(document.getParentDocumentName()).thenReturn(null);
		doReturn(false).when(document).isPersistable();

		List<String> result = evaluator.completeWithoutPrefixOrSuffix("first", customer, module, document);

		assertNotNull(result);
		assertTrue(result.contains("firstName"));
		assertFalse(result.contains("lastName"));
	}

	@Test
	void completeWithNonMatchingFragmentReturnsOnlyMatchingImplicits() {
		// Fragment that matches no attributes but matches some implicit bindings
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		doReturn(List.of()).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of());
		when(document.getParentDocumentName()).thenReturn(null);
		doReturn(false).when(document).isPersistable();

		// "changed" only matches Bean.CHANGED_KEY and NOT_CHANGED_KEY won't start with "changed"
		List<String> result = evaluator.completeWithoutPrefixOrSuffix("changed", customer, module, document);

		assertNotNull(result);
		assertTrue(result.contains(Bean.CHANGED_KEY));
		assertFalse(result.contains(Bean.NOT_CHANGED_KEY)); // "notChanged" doesn't start with "changed"
	}

	@Test
	void completeWithChildDocumentParentFragmentAddsParentName() {
		// parentDocumentName != documentName → ChildBean path → PARENT_NAME added
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		doReturn(List.of()).when(document).getAllAttributes(customer);
		when(document.getConditionNames()).thenReturn(Set.of());
		when(document.getParentDocumentName()).thenReturn("SomeParent");
		when(document.getName()).thenReturn("ChildDoc");
		doReturn(false).when(document).isPersistable();

		List<String> result = evaluator.completeWithoutPrefixOrSuffix(null, customer, module, document);

		assertTrue(result.contains(ChildBean.PARENT_NAME));
		assertFalse(result.contains(HierarchicalBean.PARENT_ID));
	}

	// ---- validateWithoutPrefixOrSuffix — type incompatibility paths ----

	@Test
	void validateWithConditionBindingAndIncompatibleReturnTypeReturnsError() {
		// "myCondition" resolves to Boolean condition; incompatible with String returnType
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getCondition("myCondition")).thenReturn(mock(Condition.class));

		String result = evaluator.validateWithoutPrefixOrSuffix("myCondition", String.class, customer, module, document);

		assertNotNull(result);
		assertTrue(result.contains("incompatible"), "Error message should mention incompatible type");
	}

	@Test
	void validateWithImplicitBindingAndIncompatibleReturnTypeReturnsError() {
		// "bizId" is an implicit String attribute; incompatible with Integer returnType
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getCondition(Bean.DOCUMENT_ID)).thenReturn(null);
		when(document.getAttribute(Bean.DOCUMENT_ID)).thenReturn(null);
		when(document.getExtends()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("test");
		when(document.getName()).thenReturn("TestDoc");
		when(customer.getModule("test")).thenReturn(module);

		String result = evaluator.validateWithoutPrefixOrSuffix(Bean.DOCUMENT_ID, Integer.class, customer, module, document);

		assertNotNull(result);
		assertTrue(result.contains("incompatible"), "Error message should mention incompatible type");
	}

	// ---- completeWithoutPrefixOrSuffix — compound binding paths ----

	@Test
	void completeWithCompoundBindingThatThrowsReturnsEmpty() {
		// "badAttr." has non-empty prefix → getMetaDataForBinding throws → caught → returns empty
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getAttribute("badAttr")).thenReturn(null);
		when(document.getExtends()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("test");
		when(document.getName()).thenReturn("TestDoc");

		List<String> result = evaluator.completeWithoutPrefixOrSuffix("badAttr.", customer, module, document);

		assertNotNull(result);
		assertTrue(result.isEmpty(), "Should return empty list when binding prefix is unresolvable");
	}

	@Test
	void completeWithCompoundBindingToScalarAttributeReturnsEmpty() {
		// "myText." → scalar attribute, no more to complete → empty
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute textAttr = mock(Attribute.class);
		when(textAttr.getAttributeType()).thenReturn(AttributeType.text);
		when(document.getAttribute("myText")).thenReturn(textAttr);
		when(document.getExtends()).thenReturn(null);

		List<String> result = evaluator.completeWithoutPrefixOrSuffix("myText.", customer, module, document);

		assertNotNull(result);
		assertTrue(result.isEmpty(), "Scalar attribute binding should return no completions");
	}

	@Test
	void completeWithCompoundBindingToImplicitAttributeReturnsEmpty() {
		// "bizId." → implicit String attribute, nothing more to complete → empty
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(document.getAttribute(Bean.DOCUMENT_ID)).thenReturn(null);
		when(document.getExtends()).thenReturn(null);

		List<String> result = evaluator.completeWithoutPrefixOrSuffix(Bean.DOCUMENT_ID + ".", customer, module, document);

		assertNotNull(result);
		assertTrue(result.isEmpty(), "Implicit attribute binding should return no completions");
	}

	@Test
	void validateWithExplicitAttributeAndIncompatibleReturnTypeReturnsError() {
		// Explicit attribute with incompatible return type → line 64 MetaDataException
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Attribute textAttr = mock(Attribute.class);
		when(textAttr.getAttributeType()).thenReturn(AttributeType.text);
		doReturn(String.class).when(textAttr).getImplementingType();
		when(document.getAttribute("myText")).thenReturn(textAttr);
		when(document.getExtends()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("test");
		when(customer.getModule("test")).thenReturn(module);

		String result = evaluator.validateWithoutPrefixOrSuffix("myText", Integer.class, customer, module, document);

		assertNotNull(result);
		assertTrue(result.contains("incompatible"), "Should report type incompatibility for explicit attribute");
	}

	@Test
	void completeWithAssociationBindingAddsRelatedDocAttributes() {
		// "myAssoc." → Association else branch → bindingPrefix += '.' → addAttributesAndConditions
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document relatedDoc = mock(Document.class);
		Association assoc = mock(Association.class);
		// Use AttributeType.text to bypass getBeanClass call in getMetaDataForBinding
		when(assoc.getAttributeType()).thenReturn(AttributeType.text);
		when(assoc.getDocumentName()).thenReturn("RelatedDoc");
		when(document.getAttribute("myAssoc")).thenReturn(assoc);
		when(document.getExtends()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("test");
		when(customer.getModule("test")).thenReturn(module);
		when(module.getDocument(customer, "RelatedDoc")).thenReturn(relatedDoc);
		// addAttributesAndConditions iterates attributes and conditions
		doReturn(List.of()).when(relatedDoc).getAllAttributes(customer);
		when(relatedDoc.getConditionNames()).thenReturn(Set.of());

		List<String> result = evaluator.completeWithoutPrefixOrSuffix("myAssoc.", customer, module, document);

		assertNotNull(result);
		// addAttributesAndConditions adds implicit bindings (bizId etc.) to result
		assertTrue(result.contains("myAssoc.bizId"), "Should include implicit attribute from related document");
	}

	@Test
	void completeWithCollectionBindingOpenBracketAddsIndexOptions() {
		// "myCol[" → Collection instanceof → simpleBindingFragment=null → add [0]-[9]
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document relatedDoc = mock(Document.class);
		Collection coll = mock(Collection.class);
		// Use AttributeType.text to bypass getBeanClass call in getMetaDataForBinding
		when(coll.getAttributeType()).thenReturn(AttributeType.text);
		when(coll.getDocumentName()).thenReturn("RelatedDoc");
		when(document.getAttribute("myCol")).thenReturn(coll);
		when(document.getExtends()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("test");
		when(customer.getModule("test")).thenReturn(module);
		when(module.getDocument(customer, "RelatedDoc")).thenReturn(relatedDoc);

		List<String> result = evaluator.completeWithoutPrefixOrSuffix("myCol[", customer, module, document);

		assertNotNull(result);
		assertTrue(result.contains("myCol[0]"), "Should offer index 0 for collection binding");
		assertTrue(result.contains("myCol[9]"), "Should offer index 9 for collection binding");
		assertFalse(result.contains("myCol[10]"), "Should not offer index 10");
	}

	@Test
	void completeWithCollectionBindingIntegerFragmentAddsClosedIndex() {
		// "myCol[1" → Collection instanceof, simpleBindingFragment="1" → Integer.parseInt succeeds → "myCol[1]"
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document relatedDoc = mock(Document.class);
		Collection coll = mock(Collection.class);
		when(coll.getAttributeType()).thenReturn(AttributeType.text);
		when(coll.getDocumentName()).thenReturn("RelatedDoc");
		when(document.getAttribute("myCol")).thenReturn(coll);
		when(document.getExtends()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("test");
		when(customer.getModule("test")).thenReturn(module);
		when(module.getDocument(customer, "RelatedDoc")).thenReturn(relatedDoc);

		List<String> result = evaluator.completeWithoutPrefixOrSuffix("myCol[1", customer, module, document);

		assertNotNull(result);
		assertTrue(result.contains("myCol[1]"), "Should offer closed index for numeric fragment");
	}

	@Test
	void completeWithCollectionBindingNonIntegerFragmentReturnsEmpty() {
		// "myCol[abc" → Collection instanceof, simpleBindingFragment="abc" → NumberFormatException → empty
		TestEvaluator evaluator = new TestEvaluator();
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		Document relatedDoc = mock(Document.class);
		Collection coll = mock(Collection.class);
		when(coll.getAttributeType()).thenReturn(AttributeType.text);
		when(coll.getDocumentName()).thenReturn("RelatedDoc");
		when(document.getAttribute("myCol")).thenReturn(coll);
		when(document.getExtends()).thenReturn(null);
		when(document.getOwningModuleName()).thenReturn("test");
		when(customer.getModule("test")).thenReturn(module);
		when(module.getDocument(customer, "RelatedDoc")).thenReturn(relatedDoc);

		List<String> result = evaluator.completeWithoutPrefixOrSuffix("myCol[abc", customer, module, document);

		assertNotNull(result);
		assertTrue(result.isEmpty(), "Non-integer fragment should produce no completions");
	}
}
