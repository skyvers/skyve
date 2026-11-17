package modules.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.ModuleDocument;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Binder;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import customers.test.modules.test.domain.OverrideProxiedOverride;
import modules.test.OverrideEnumExtended.OverrideEnumExtendedExtension;
import modules.test.OverrideProxied.OverrideProxiedFactory;
import modules.test.domain.OverrideEnumJoinedSub;
import modules.test.domain.OverrideEnumJoinedSubVanilla;
import modules.test.domain.OverrideProxied;
import modules.test.domain.TestDocument;

/**
 * Customer override behaviour that is wrong at the moment, each test disabled until its fix lands.
 * The fixtures are the <code>Override*</code> documents of the test module and their overrides in
 * <code>customers/test/modules/test/</code>, as for {@link CustomerOverrideH2Test}.
 */
@SuppressWarnings("static-method")
class CustomerOverridePendingH2Test extends AbstractCustomerOverrideH2Test {
	/** The second customer, whose overrides combine with the test customer's. */
	private static final String TEST2_CUSTOMER = "test2";

	/** The enum constant's code, read whatever class declares the enum. */
	private static String code(Object enumeration) {
		return (enumeration == null) ? null : ((Enumeration) enumeration).toCode();
	}

	/**
	 * docs/typed-metadata-references.md and the {@link ModuleDocument#toDocument(Customer)} Javadoc say
	 * that it throws {@link MetaDataException} when the name does not resolve for the customer, but it
	 * lets the repository's IllegalArgumentException through.
	 */
	@Test
	@Disabled("toDocument throws IllegalArgumentException, not the documented MetaDataException — fix pending")
	void toDocumentThrowsMetaDataExceptionForAMissingDocument() {
		Customer test = runAs(TEST_CUSTOMER);
		assertThrows(MetaDataException.class, () -> TestDocument.OVERRIDE_REMOVED.toDocument(test));
	}

	/** Collects the bindings of a view's text fields, including default widgets of text attributes. */
	private static final class TextBindings extends NoOpViewVisitor {
		final Set<String> bindings = new TreeSet<>();

		TextBindings(CustomerImpl customer, ModuleImpl module, DocumentImpl document, ViewImpl view) {
			super(customer, module, document, view, "desktop");
		}

		@Override
		public void visitTextField(TextField text, boolean parentVisible, boolean parentEnabled) {
			bindings.add(text.getBinding());
		}
	}

	/** The customer's edit view of the document, and the bindings of its text fields. */
	private static Map.Entry<View, Set<String>> editView(Customer customer, ModuleDocument document) {
		Document d = document.toDocument(customer);
		View view = d.getView(null, customer, ViewType.edit.toString());
		assertNotNull(view, document.documentName());
		TextBindings visitor = new TextBindings((CustomerImpl) customer,
												(ModuleImpl) customer.getModule(d.getOwningModuleName()),
												(DocumentImpl) d,
												(ViewImpl) view);
		visitor.visit();
		return Map.entry(view, visitor.bindings);
	}

	/**
	 * Outside dev mode, MutableCachedRepository caches a scaffolded view, so that the first customer to
	 * scaffold a view would give it to every other customer if it were cached under the vanilla view
	 * key. A customer's override can add or drop attributes - the test customer's OverrideAdds adds
	 * customerText, and neither customer has an edit view of OverrideAdds or OverrideLength - so each
	 * customer gets its own.
	 */
	@Test
	void aScaffoldedViewIsNotSharedBetweenCustomers() {
		Customer test = runAs(TEST_CUSTOMER);
		Map.Entry<View, Set<String>> testLength = editView(test, TestDocument.OVERRIDE_LENGTH);
		Map.Entry<View, Set<String>> testAdds = editView(test, TestDocument.OVERRIDE_ADDS);
		Customer bizhub = runAs(CUSTOMER);
		Map.Entry<View, Set<String>> bizhubLength = editView(bizhub, TestDocument.OVERRIDE_LENGTH);
		Map.Entry<View, Set<String>> bizhubAdds = editView(bizhub, TestDocument.OVERRIDE_ADDS);

		assertAll(() -> assertNotSame(testLength.getKey(), bizhubLength.getKey()),
					() -> assertNotSame(testAdds.getKey(), bizhubAdds.getKey()),
					() -> assertEquals(Set.of("text", "customerText"), testAdds.getValue()),
					() -> assertEquals(Set.of("text"), bizhubAdds.getValue()));
	}

	/**
	 * OverrideProxied extends the mapped OverrideProxy, which shares its table, and has the
	 * hand-written OverrideProxiedExtension, as admin.User does with admin.UserProxy and
	 * UserExtension. The test customer's override adds customerText. The generated
	 * OverrideProxiedOverride extends OverrideProxiedExtension, but the customer mapping maps
	 * OverrideProxiedExtension for testtestOverrideProxied, without customerText.
	 */
	@Test
	void anOverrideOfASubclassOfAMappedDocumentWithAnExtensionSavesItsAddedAttribute() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideProxiedOverride bean = newInstance(TestDocument.OVERRIDE_PROXIED);
		bean.setText("text");
		bean.setSubText("sub");
		bean.setCustomerText("customer");

		OverrideProxied result = saveAndRetrieve(TestDocument.OVERRIDE_PROXIED, bean);

		assertAll(() -> assertSame(OverrideProxiedOverride.class, beanClass(TestDocument.OVERRIDE_PROXIED, test)),
					() -> assertSame(OverrideProxiedOverride.class, result.getClass()),
					() -> assertEquals("customer", ((OverrideProxiedOverride) result).getCustomerText()));
	}

	/**
	 * DataBuilder only uses a factory method whose return type is assignable to the customer's bean
	 * class, so for the test customer, whose OverrideProxied bean class is OverrideProxiedOverride, it
	 * skips OverrideProxiedFactory.crudInstance(), which returns OverrideProxiedExtension, and builds a
	 * random bean instead. UserFactory has the same shape, so a customer that overrides admin.User
	 * gets random users from DataBuilder.
	 */
	@Test
	void aDataFactoryAppliesToACustomersOverrideClass() {
		runAs(TEST_CUSTOMER);
		OverrideProxied bean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.OVERRIDE_PROXIED);

		assertAll(() -> assertSame(OverrideProxiedOverride.class, bean.getClass()),
					() -> assertEquals(OverrideProxiedFactory.CRUD_TEXT, bean.getText()));
	}

	/**
	 * The test customer's override of the joined subclass OverrideEnumJoinedSub leaves out status.
	 * The vanilla mapping maps OverrideEnumJoinedSubVanilla, which declares status, but the generator
	 * only makes a narrowed document's class abstract when it extends no document, so
	 * OverrideEnumJoinedSub stays concrete and DocumentImpl.getBeanClass gives it to other customers,
	 * without status.
	 */
	@Test
	void aNarrowedSubclassKeepsTheDroppedEnumForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideEnumJoinedSub bean = newInstance(TestDocument.OVERRIDE_ENUM_JOINED_SUB);
		bean.setText("base");
		bean.setSubText("sub");

		assertAll(() -> assertSame(OverrideEnumJoinedSubVanilla.class, beanClass(TestDocument.OVERRIDE_ENUM_JOINED_SUB, bizhub)),
					() -> assertInstanceOf(OverrideEnumJoinedSubVanilla.class, bean));
		((OverrideEnumJoinedSubVanilla) bean).setStatus(OverrideEnumJoinedSubVanilla.Status.closed);
		OverrideEnumJoinedSub result = saveAndRetrieve(TestDocument.OVERRIDE_ENUM_JOINED_SUB, bean);
		assertSame(OverrideEnumJoinedSubVanilla.Status.closed, ((OverrideEnumJoinedSubVanilla) result).getStatus());
	}

	/**
	 * The test customer's override of OverrideNarrowedLinked leaves out dropped, so its vanilla class
	 * is abstract and OverrideNarrowedLinkedVanilla declares dropped. The test2 customer overrides
	 * OverrideLinkTarget, which OverrideNarrowedLinked references, so test2's mapping copies
	 * OverrideNarrowedLinked - mapping the abstract OverrideNarrowedLinked with dropped, which only
	 * OverrideNarrowedLinkedVanilla has, so the SessionFactory does not start. The fixture is test2's
	 * OverrideLinkTarget in customers/test2.
	 */
	@Test
	void aNarrowedDocumentCopiedIntoAnotherCustomersMappingSavesItsDroppedAttribute() throws Exception {
		runAs(TEST2_CUSTOMER);
		Bean target = newInstance(TestDocument.OVERRIDE_LINK_TARGET);
		Binder.set(target, "text", "target");
		PersistentBean bean = newInstance(TestDocument.OVERRIDE_NARROWED_LINKED);
		Binder.set(bean, "text", "text");
		Binder.set(bean, "dropped", "dropped");
		Binder.set(bean, "target", target);

		PersistentBean result = saveAndRetrieve(TestDocument.OVERRIDE_NARROWED_LINKED, bean);

		assertAll(() -> assertEquals("dropped", Binder.get(result, "dropped")),
					() -> assertEquals("target", Binder.get(result, "target.text")));
	}

	/**
	 * In the joined hierarchy OverrideEnumJoinedBase and OverrideEnumJoinedSub, the test customer's
	 * override of OverrideEnumJoinedSub leaves out the enum status, and the test2 customer overrides
	 * OverrideEnumJoinedBase without changes, so test2's mapping copies the hierarchy. Its mapping of
	 * OverrideEnumJoinedSub's status names an enum class that is not generated
	 * (<code>customers.test2.modules.test.domain.OverrideEnumJoinedSubOverride$Status</code>), so the
	 * SessionFactory does not start. The fixture is test2's OverrideEnumJoinedBase in customers/test2.
	 */
	@Test
	void aNarrowedJoinedSubclassCopiedIntoAnotherCustomersMappingSavesItsEnum() throws Exception {
		runAs(TEST2_CUSTOMER);
		PersistentBean bean = newInstance(TestDocument.OVERRIDE_ENUM_JOINED_SUB);
		Binder.set(bean, "text", "base");
		Binder.set(bean, "subText", "sub");
		Binder.convertAndSet(bean, "status", "closed");

		PersistentBean result = saveAndRetrieve(TestDocument.OVERRIDE_ENUM_JOINED_SUB, bean);
		List<PersistentBean> polymorphic = CORE.getPersistence().newDocumentQuery(TestDocument.OVERRIDE_ENUM_JOINED_BASE).beanResults();

		assertAll(() -> assertEquals("closed", code(Binder.get(result, "status"))),
					() -> assertEquals(1, polymorphic.size(), polymorphic.toString()),
					() -> assertEquals("closed", code(Binder.get(polymorphic.get(0), "status"))));
	}

	/**
	 * The test customer's override of OverrideEnumExtended leaves out the enum status, so the
	 * hand-written OverrideEnumExtendedExtension extends OverrideEnumExtendedVanilla, which declares
	 * status. The test2 customer's override keeps status, so its generated
	 * OverrideEnumExtendedOverride extends the extension and declares status again, with its own enum
	 * type, which clashes with OverrideEnumExtendedVanilla.getStatus(), so skyve-war does not compile.
	 * The fixture is test2's OverrideEnumExtended in customers/test2.
	 */
	@Test
	void anOverrideKeepingAnEnumAnotherCustomerDropsSavesIt() throws Exception {
		Customer test2 = runAs(TEST2_CUSTOMER);
		PersistentBean bean = newInstance(TestDocument.OVERRIDE_ENUM_EXTENDED);
		Binder.set(bean, "text", "text");
		Binder.convertAndSet(bean, "status", "closed");

		PersistentBean result = saveAndRetrieve(TestDocument.OVERRIDE_ENUM_EXTENDED, bean);

		assertAll(() -> assertEquals("customers.test2.modules.test.domain.OverrideEnumExtendedOverride",
										beanClass(TestDocument.OVERRIDE_ENUM_EXTENDED, test2).getName()),
					() -> assertEquals("closed", code(Binder.get(result, "status"))),
					() -> assertEquals("Enum extended text", ((OverrideEnumExtendedExtension) result).describe()));
	}
}
