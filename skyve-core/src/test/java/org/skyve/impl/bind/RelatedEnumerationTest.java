package org.skyve.impl.bind;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.List;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.types.Decimal2;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.impl.metadata.model.document.field.Enumeration.EnumeratedValue;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Extends;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;

import jakarta.el.ELProcessor;
import jakarta.el.PropertyNotFoundException;

/**
 * Tests that an EL expression can name the enumerations of the documents its root document is related to.
 */
@SuppressWarnings({"static-method", "java:S112"})
class RelatedEnumerationTest {
	private static final String MODULE = "test";

	public enum Colour { RED, BLUE }
	public enum Shade { RED, DARK }
	public enum Size { SMALL }

	/** A stand-in for a generated bean class of the Root document with no enumerations of its own. */
	public static final class RootBean extends DynamicBean {
		private static final long serialVersionUID = 1L;

		public RootBean() {
			super(MODULE, "Root", new HashMap<>());
		}
	}

	/** A stand-in for a generated bean class of the Root document that declares an enumeration. */
	public static final class LocalEnumRootBean extends DynamicBean {
		private static final long serialVersionUID = 1L;

		public enum Colour { GREEN }

		public LocalEnumRootBean() {
			super(MODULE, "Root", new HashMap<>());
		}
	}

	private Customer customer;
	private Module module;

	@BeforeEach
	void setUp() {
		customer = mock(Customer.class);
		module = mock(Module.class);
		when(customer.getModule(MODULE)).thenReturn(module);
	}

	@AfterEach
	void tearDown() throws Exception {
		ThreadLocalPersistenceTestUtil.clearThreadLocalPersistence();
		ELExpressionEvaluator.ENUM_BEANS.remove(RootBean.class);
		ELExpressionEvaluator.ENUM_BEANS.remove(LocalEnumRootBean.class);
		ELExpressionEvaluator.ENUM_BEANS.remove(DynamicBean.class);
	}

	// ---- metadata fixtures ----

	private DocumentImpl document(String name, Attribute... attributes) {
		return document(module, MODULE, name, attributes);
	}

	private DocumentImpl document(Module owningModule, String moduleName, String name, Attribute... attributes) {
		DocumentImpl result = mock(DocumentImpl.class);
		when(result.getOwningModuleName()).thenReturn(moduleName);
		when(result.getName()).thenReturn(name);
		doReturn(List.of(attributes)).when(result).getAttributes();
		doReturn(List.of(attributes)).when(result).getAllAttributes(customer);
		for (Attribute attribute : attributes) {
			String attributeName = attribute.getName();
			if (attributeName != null) {
				when(result.getAttribute(attributeName)).thenReturn(attribute);
			}
		}
		when(owningModule.getDocument(customer, name)).thenReturn(result);
		return result;
	}

	private static Relation relation(String attributeName, String documentName) {
		Relation result = mock(Relation.class);
		when(result.getName()).thenReturn(attributeName);
		when(result.getDocumentName()).thenReturn(documentName);
		return result;
	}

	private static Enumeration enumeration(String attributeName, String typeName, String owningDocumentName, Class<?> enumClass) {
		Enumeration result = mock(Enumeration.class);
		when(result.getName()).thenReturn(attributeName);
		when(result.toJavaIdentifier()).thenReturn(typeName);
		when(result.getEncapsulatingClassName()).thenReturn("modules.test.domain." + owningDocumentName);
		doReturn(enumClass).when(result).getImplementingType();
		return result;
	}

	private static Enumeration ungeneratedEnumeration(String attributeName, String typeName, String owningDocumentName, String valueName, String code) {
		Enumeration result = mock(Enumeration.class);
		EnumeratedValue value = new EnumeratedValue();
		value.setName(valueName);
		value.setCode(code);
		when(result.getName()).thenReturn(attributeName);
		when(result.toJavaIdentifier()).thenReturn(typeName);
		when(result.getValues()).thenReturn(List.of(value));
		when(result.getEncapsulatingClassName()).thenReturn("modules.test.domain." + owningDocumentName);
		when(result.getImplementingType()).thenThrow(new MetaDataException("Not generated", new ClassNotFoundException(typeName)));
		return result;
	}

	// ---- relatedEnumeration() ----

	@Test
	void nothingIsFoundWithoutRelations() {
		DocumentImpl root = document("Root");
		assertNull(ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void theRootDocumentsOwnEnumerationsAreNotConsidered() {
		DocumentImpl root = document("Root", enumeration("colour", "Colour", "Root", Colour.class));
		assertNull(ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void anEnumerationOneRelationAwayIsFound() {
		Enumeration colour = enumeration("colour", "Colour", "Other", Colour.class);
		document("Other", enumeration("size", "Size", "Other", Size.class), colour);
		DocumentImpl root = document("Root", relation("other", "Other"));
		assertSame(colour, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
		assertNull(ELExpressionEvaluator.relatedEnumeration(customer, root, "Shade"));
	}

	@Test
	void anEnumerationTwoRelationsAwayIsFound() {
		Enumeration colour = enumeration("colour", "Colour", "Far", Colour.class);
		document("Far", colour);
		document("Near", relation("far", "Far"));
		DocumentImpl root = document("Root", relation("near", "Near"));
		assertSame(colour, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void theClosestEnumerationWins() {
		Enumeration near = enumeration("colour", "Colour", "Near", Colour.class);
		document("Far", enumeration("colour", "Colour", "Far", Shade.class));
		document("Near", relation("far", "Far"), near);
		DocumentImpl root = document("Root", relation("near", "Near"));
		assertSame(near, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void theClosestEnumerationWinsWhateverOrderTheRelationsAreDeclaredIn() {
		Enumeration near = enumeration("colour", "Colour", "Near", Colour.class);
		document("Far", enumeration("colour", "Colour", "Far", Shade.class));
		document("Middle", relation("far", "Far"));
		document("Near", near);
		DocumentImpl root = document("Root", relation("middle", "Middle"), relation("near", "Near"));
		assertSame(near, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void theSameEnumerationReachedTwiceAtTheSameDistanceIsNotAmbiguous() {
		Enumeration first = enumeration("colour", "Colour", "Shared", Colour.class);
		// an enumeration that refers to the one above shares its class
		Enumeration second = enumeration("tint", "Colour", "Shared", Colour.class);
		document("A", first);
		document("B", second);
		DocumentImpl root = document("Root", relation("a", "A"), relation("b", "B"));
		assertSame(first, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void theSameHandCodedEnumerationReachedTwiceIsNotAmbiguous() {
		Enumeration first = enumeration("colour", "Colour", "A", Colour.class);
		when(first.getImplementingEnumClassName()).thenReturn(Colour.class.getName());
		Enumeration second = enumeration("colour", "Colour", "B", Colour.class);
		when(second.getImplementingEnumClassName()).thenReturn(Colour.class.getName());
		document("A", first);
		document("B", second);
		DocumentImpl root = document("Root", relation("a", "A"), relation("b", "B"));
		assertSame(first, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void differentEnumerationsAtTheSameDistanceAreAmbiguous() {
		document("A", enumeration("colour", "Colour", "A", Colour.class));
		document("B", enumeration("colour", "Colour", "B", Shade.class));
		DocumentImpl root = document("Root", relation("a", "A"), relation("b", "B"));
		MetaDataException e = assertThrows(MetaDataException.class, () -> ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
		String message = e.getMessage();
		assertTrue(message.contains("Colour"), message);
		assertTrue(message.contains("test.Root"), message);
		assertTrue(message.contains("test.A"), message);
		assertTrue(message.contains("test.B"), message);
	}

	@Test
	void anAmbiguityFurtherAwayThanAMatchDoesNotMatter() {
		Enumeration near = enumeration("colour", "Colour", "Near", Colour.class);
		document("A", enumeration("colour", "Colour", "A", Shade.class));
		document("B", enumeration("colour", "Colour", "B", Size.class));
		document("Near", relation("a", "A"), relation("b", "B"), near);
		DocumentImpl root = document("Root", relation("near", "Near"));
		assertSame(near, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void cyclesAreFollowedOnce() {
		DocumentImpl other = document("Other", relation("root", "Root"), relation("self", "Other"));
		DocumentImpl root = document("Root", relation("other", "Other"), relation("self", "Root"));
		assertNull(ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
		verify(other, times(1)).getAllAttributes(customer);
		verify(root, never()).getAllAttributes(customer);
	}

	@Test
	void aDocumentReachedByTwoPathsIsSearchedOnce() {
		Enumeration colour = enumeration("colour", "Colour", "D", Colour.class);
		DocumentImpl d = document("D", colour);
		document("B", relation("d", "D"));
		document("C", relation("d", "D"));
		DocumentImpl root = document("Root", relation("b", "B"), relation("c", "C"));
		assertSame(colour, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
		verify(d, times(1)).getAllAttributes(customer);
	}

	@Test
	void inheritedRelationsAreFollowedThroughTheModuleThatDeclaresThem() {
		Module baseModule = mock(Module.class);
		when(customer.getModule("base")).thenReturn(baseModule);
		Enumeration colour = enumeration("colour", "Colour", "Other", Colour.class);
		// Other is only known to the base module
		document(baseModule, "base", "Other", colour);
		DocumentImpl base = document(baseModule, "base", "Base", relation("other", "Other"));
		DocumentImpl root = document("Root");
		Extends inherits = mock(Extends.class);
		when(inherits.getDocumentName()).thenReturn("Base");
		when(root.getExtends()).thenReturn(inherits);
		when(module.getDocument(customer, "Base")).thenReturn(base);
		assertSame(colour, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
		verify(module, never()).getDocument(customer, "Other");
	}

	@Test
	void aLocalDeclarationIsPreferredToAnImportedOneWithinADocument() {
		Enumeration imported = enumeration("tint", "Colour", "Elsewhere", Shade.class);
		when(imported.getAttributeRef()).thenReturn("colour");
		Enumeration local = enumeration("colour", "Colour", "Other", Colour.class);
		document("Other", imported, local);
		DocumentImpl root = document("Root", relation("other", "Other"));
		assertSame(local, ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	@Test
	void dynamicEnumerationsAreIgnored() {
		Enumeration dynamic = enumeration("colour", "Colour", "Other", String.class);
		when(Boolean.valueOf(dynamic.isDynamic())).thenReturn(Boolean.TRUE);
		document("Other", dynamic);
		DocumentImpl root = document("Root", relation("other", "Other"));
		assertNull(ELExpressionEvaluator.relatedEnumeration(customer, root, "Colour"));
	}

	// ---- validation ----

	private DocumentImpl rootRelatedToColour() {
		Enumeration colour = enumeration("colour", "Colour", "Other", Colour.class);
		document("Other", colour);
		return document("Root", relation("other", "Other"));
	}

	@Test
	void validationResolvesAnEnumerationOfARelatedDocument() {
		DocumentImpl root = rootRelatedToColour();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		assertEquals(Colour.RED, ELExpressionEvaluator.newSkyveValidationProcessor(customer, root).eval("Colour.RED"));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("Colour.RED", Colour.class, customer, module, root));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("bean.other.colour eq Colour.RED", Boolean.class, customer, module, root));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("bean.other.colour ne Colour.BLUE and not empty bean.other", Boolean.class, customer, module, root));
	}

	@Test
	void validationRejectsAnUnknownValueOfARelatedEnumeration() {
		DocumentImpl root = rootRelatedToColour();
		String error = new ELExpressionEvaluator(true).validateWithoutPrefixOrSuffix("bean.other.colour eq Colour.PURPLE", Boolean.class, customer, module, root);
		assertNotNull(error);
	}

	@Test
	void validationResolvesARelatedEnumerationBeforeItsClassIsGenerated() {
		document("Other", ungeneratedEnumeration("colour", "Colour", "Other", "RED", "red"));
		DocumentImpl root = document("Root", relation("other", "Other"));
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		assertEquals("red", ELExpressionEvaluator.newSkyveValidationProcessor(customer, root).eval("Colour.RED"));
		assertNull(evaluator.validateWithoutPrefixOrSuffix("bean.other.colour == Colour.RED", Boolean.class, customer, module, root));
		assertNotNull(evaluator.validateWithoutPrefixOrSuffix("Colour.BLUE", null, customer, module, root));
	}

	@Test
	void validationPrefersTheRootDocumentsOwnEnumeration() {
		document("Other", enumeration("colour", "Colour", "Other", Shade.class));
		DocumentImpl root = document("Root", relation("other", "Other"), enumeration("colour", "Colour", "Root", Colour.class));
		ELProcessor processor = ELExpressionEvaluator.newSkyveValidationProcessor(customer, root);
		assertEquals(Colour.BLUE, processor.eval("Colour.BLUE"));
		assertUnresolved("DARK", () -> processor.eval("Colour.DARK"));
	}

	@Test
	void validationCannotTellThatTheRootDocumentsEnumerationShadowsARelatedOneOfTheSameName() {
		// A known limit. Other.colour is a Shade, but Colour names the root document's own enumeration.
		// Comparing the two is legal EL that is never true, so only a value Colour lacks is reported.
		document("Other", enumeration("colour", "Colour", "Other", Shade.class));
		DocumentImpl root = document("Root", relation("other", "Other"), enumeration("tint", "Colour", "Root", Colour.class));
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(true);
		assertNull(evaluator.validateWithoutPrefixOrSuffix("bean.other.colour eq Colour.RED", Boolean.class, customer, module, root));
		assertNotNull(evaluator.validateWithoutPrefixOrSuffix("bean.other.colour eq Colour.DARK", Boolean.class, customer, module, root));
	}

	@Test
	void validationStillReportsAnIdentifierNothingDeclares() {
		DocumentImpl root = rootRelatedToColour();
		String error = new ELExpressionEvaluator(true).validateWithoutPrefixOrSuffix("Shade.RED", null, customer, module, root);
		assertNotNull(error);
		assertTrue(error.contains("Shade"), error);
	}

	@Test
	void validationReportsAnAmbiguousEnumeration() {
		document("A", enumeration("colour", "Colour", "A", Colour.class));
		document("B", enumeration("colour", "Colour", "B", Shade.class));
		DocumentImpl root = document("Root", relation("a", "A"), relation("b", "B"));
		String error = new ELExpressionEvaluator(true).validateWithoutPrefixOrSuffix("Colour.RED", null, customer, module, root);
		assertNotNull(error);
		assertTrue(error.contains("ambiguous"), error);
	}

	@Test
	void validationDoesNotSearchForVariablesOrImportedClasses() {
		DocumentImpl other = document("Other", enumeration("colour", "Colour", "Other", Colour.class));
		DocumentImpl root = document("Root", relation("other", "Other"));
		ELProcessor processor = ELExpressionEvaluator.newSkyveValidationProcessor(customer, root);
		assertSame(root, processor.eval("bean"));
		assertNotNull(processor.eval("user"));
		assertNotNull(processor.eval("stash"));
		assertEquals(Decimal2.ONE, processor.eval("Decimal2.ONE"));
		assertEquals(Boolean.TRUE, processor.eval("Boolean.TRUE"));
		assertEquals(Integer.valueOf(Integer.MAX_VALUE), processor.eval("Integer.MAX_VALUE"));
		// the related document was never asked for its enumerations
		verify(other, never()).getAllAttributes(customer);
	}

	@Test
	void validationWithoutADocumentResolvesNoEnumerations() {
		assertUnresolved("Colour", () -> ELExpressionEvaluator.newSkyveValidationProcessor(null, null).eval("Colour.RED"));
	}

	// ---- evaluation ----

	private void bindUser(User user) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		when(persistence.getStash()).thenReturn(new TreeMap<>());
		when(persistence.getUser()).thenReturn(user);
		ThreadLocalPersistenceTestUtil.setThreadLocalPersistence(persistence);
	}

	private User bindUserOfCustomer() throws Exception {
		User user = mock(User.class);
		when(user.getCustomer()).thenReturn(customer);
		bindUser(user);
		return user;
	}

	@Test
	void evaluationResolvesAnEnumerationOfARelatedDocument() throws Exception {
		rootRelatedToColour();
		bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		assertEquals(Colour.RED, evaluator.evaluateWithoutPrefixOrSuffix("Colour.RED", new RootBean()));
		assertEquals(Boolean.TRUE, evaluator.evaluateWithoutPrefixOrSuffix("Colour.BLUE ne Colour.RED", new RootBean()));
		assertEquals(Colour.BLUE, org.skyve.util.ExpressionEvaluator.evaluate("{rtel:Colour.BLUE}", new RootBean()));
		assertEquals(Colour.BLUE, org.skyve.util.ExpressionEvaluator.evaluate("{el:Colour.BLUE}", new RootBean()));
	}

	@Test
	void evaluationRemembersWhatItFoundForABeanClass() throws Exception {
		rootRelatedToColour();
		User user = bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		assertTrue(ELExpressionEvaluator.ENUM_BEANS.get(RootBean.class).isEmpty());
		assertEquals(Colour.RED, evaluator.evaluateWithoutPrefixOrSuffix("Colour.RED", new RootBean()));
		assertEquals(Colour.class, ELExpressionEvaluator.ENUM_BEANS.get(RootBean.class).get("Colour").getKlass());

		clearInvocations(user, customer, module);
		assertEquals(Colour.BLUE, evaluator.evaluateWithoutPrefixOrSuffix("Colour.BLUE", new RootBean()));
		verifyNoInteractions(customer, module);
		verify(user, never()).getCustomer();
	}

	@Test
	void evaluationDoesNotRememberAnythingForTheSharedDynamicBeanClass() throws Exception {
		rootRelatedToColour();
		bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		Bean bean = new DynamicBean(MODULE, "Root", new HashMap<>());
		assertEquals(Colour.RED, evaluator.evaluateWithoutPrefixOrSuffix("Colour.RED", bean));
		assertTrue(ELExpressionEvaluator.ENUM_BEANS.get(DynamicBean.class).isEmpty());

		// another dynamic document of the same class is searched for itself
		document("Unrelated");
		Bean unrelated = new DynamicBean(MODULE, "Unrelated", new HashMap<>());
		assertUnresolved("Colour", () -> evaluator.evaluateWithoutPrefixOrSuffix("Colour.RED", unrelated));
		clearInvocations(customer);
		assertEquals(Colour.BLUE, evaluator.evaluateWithoutPrefixOrSuffix("Colour.BLUE", bean));
		verify(customer, times(2)).getModule(MODULE);
	}

	@Test
	void evaluationPrefersTheBeanClassesOwnEnumeration() throws Exception {
		document("Other", enumeration("colour", "Colour", "Other", Colour.class));
		document("Root", relation("other", "Other"));
		User user = bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		assertEquals(LocalEnumRootBean.Colour.GREEN, evaluator.evaluateWithoutPrefixOrSuffix("Colour.GREEN", new LocalEnumRootBean()));
		verify(user, never()).getCustomer();
		// the bean class's own enumeration is all that is known for it
		assertEquals(1, ELExpressionEvaluator.ENUM_BEANS.get(LocalEnumRootBean.class).size());
		assertEquals(LocalEnumRootBean.Colour.class, ELExpressionEvaluator.ENUM_BEANS.get(LocalEnumRootBean.class).get("Colour").getKlass());
	}

	@Test
	void evaluationDoesNotSearchForVariablesOrImportedClasses() throws Exception {
		rootRelatedToColour();
		User user = bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		RootBean bean = new RootBean();
		assertSame(bean, evaluator.evaluateWithoutPrefixOrSuffix("bean", bean));
		assertSame(user, evaluator.evaluateWithoutPrefixOrSuffix("user", bean));
		assertNotNull(evaluator.evaluateWithoutPrefixOrSuffix("stash", bean));
		assertEquals(Decimal2.ONE, evaluator.evaluateWithoutPrefixOrSuffix("Decimal2.ONE", bean));
		assertEquals(Boolean.TRUE, evaluator.evaluateWithoutPrefixOrSuffix("Boolean.TRUE", bean));
		verify(user, never()).getCustomer();
	}

	@Test
	void evaluationStillFailsForAnIdentifierNothingDeclares() throws Exception {
		rootRelatedToColour();
		bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		Exception e = assertThrows(Exception.class, () -> evaluator.evaluateWithoutPrefixOrSuffix("Shade.RED", new RootBean()));
		assertTrue(e.getMessage().contains("Shade"), e.getMessage());
		assertTrue(ELExpressionEvaluator.ENUM_BEANS.get(RootBean.class).isEmpty());
	}

	@Test
	void evaluationDoesNotResolveAnEnumerationWhoseClassCannotBeLoaded() throws Exception {
		document("Other", ungeneratedEnumeration("colour", "Colour", "Other", "RED", "red"));
		document("Root", relation("other", "Other"));
		bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		assertUnresolved("Colour", () -> evaluator.evaluateWithoutPrefixOrSuffix("Colour.RED", new RootBean()));
		assertTrue(ELExpressionEvaluator.ENUM_BEANS.get(RootBean.class).isEmpty());
	}

	@Test
	void evaluationReportsAnAmbiguousEnumeration() throws Exception {
		document("A", enumeration("colour", "Colour", "A", Colour.class));
		document("B", enumeration("colour", "Colour", "B", Shade.class));
		document("Root", relation("a", "A"), relation("b", "B"));
		bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		Throwable t = assertThrows(Exception.class, () -> evaluator.evaluateWithoutPrefixOrSuffix("Colour.RED", new RootBean()));
		while ((t.getCause() != null) && (! (t instanceof MetaDataException))) {
			t = t.getCause();
		}
		assertTrue(t.getMessage().contains("ambiguous"), t.getMessage());
	}

	@Test
	void evaluationOfABeanThatDoesNotNameItsDocumentResolvesNoRelatedEnumerations() throws Exception {
		rootRelatedToColour();
		bindUserOfCustomer();
		ELExpressionEvaluator evaluator = new ELExpressionEvaluator(false);
		Bean noModule = mock(Bean.class);
		when(noModule.getBizDocument()).thenReturn("Root");
		assertUnresolved("Colour", () -> evaluator.evaluateWithoutPrefixOrSuffix("Colour.RED", noModule));
		Bean noDocument = mock(Bean.class);
		when(noDocument.getBizModule()).thenReturn(MODULE);
		assertUnresolved("Colour", () -> evaluator.evaluateWithoutPrefixOrSuffix("Colour.RED", noDocument));
		verify(customer, never()).getModule(any());
	}

	/**
	 * Asserts that EL cannot resolve an identifier, as opposed to failing for an unrelated reason.
	 */
	private static void assertUnresolved(String identifier, Executable evaluation) {
		PropertyNotFoundException e = assertThrows(PropertyNotFoundException.class, evaluation);
		assertTrue(e.getMessage().contains(identifier), e.getMessage());
	}
}
