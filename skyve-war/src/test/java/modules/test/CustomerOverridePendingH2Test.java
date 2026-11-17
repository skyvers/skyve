package modules.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.List;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Enumeration;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
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

	/**
	 * The test2 customer's fixtures stop the SessionFactory or the build, so they wait in
	 * src/test/pending: each test names the override it needs.
	 */
	private static final String PENDING_FIXTURE = "to run it, copy src/test/pending/customers/test2/test2.xml and the test's override into src/main/java/customers/test2 and generate the domain";

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

	/**
	 * Outside dev mode, MutableCachedRepository caches a scaffolded view under the vanilla key
	 * <code>modules/&lt;module&gt;/&lt;document&gt;/views/&lt;view&gt;</code> whatever the customer, so
	 * the first customer to scaffold a view gives it to every other customer - here the edit view of
	 * OverrideLength, which the test customer overrides without a view of its own. When the override
	 * adds or drops attributes, the other customer gets a view of attributes its document doesn't
	 * have: generation validates customers in turn and fails that way
	 * (<code>test.OverrideAdds @ customerText does not exist</code> for bizhub), which is why the test
	 * customer's overrides that add or drop attributes have their own edit views.
	 */
	@Test
	@Disabled("repository bug: a scaffolded view is cached for all customers — fix pending")
	void aScaffoldedViewIsNotSharedBetweenCustomers() {
		Customer test = runAs(TEST_CUSTOMER);
		View testView = TestDocument.OVERRIDE_LENGTH.toDocument(test).getView(null, test, ViewType.edit.toString());
		Customer bizhub = runAs(CUSTOMER);
		View bizhubView = TestDocument.OVERRIDE_LENGTH.toDocument(bizhub).getView(null, bizhub, ViewType.edit.toString());

		assertAll(() -> assertNotNull(testView),
					() -> assertNotNull(bizhubView),
					() -> assertNotSame(testView, bizhubView));
	}

	/**
	 * OverrideProxied extends the mapped OverrideProxy, which shares its table, and has the
	 * hand-written OverrideProxiedExtension, as admin.User does with admin.UserProxy and
	 * UserExtension. The test customer's override adds customerText. The generated
	 * OverrideProxiedOverride extends OverrideProxiedExtension, but the customer mapping maps
	 * OverrideProxiedExtension for testtestOverrideProxied, without customerText.
	 */
	@Test
	@Disabled("generator bug: the customer mapping of an override of a subclass of a mapped document with an Extension maps the Extension, without the added attribute — fix pending")
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
	@Disabled("DataBuilder bug: a data factory is skipped for a customer whose bean class is a generated Override — fix pending")
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
	@Disabled("generator bug: a narrowed subclass's domain class is not abstract, so other customers do not get its Vanilla class — fix pending")
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
	 * OverrideLinkTarget in src/test/pending.
	 */
	@Test
	@Disabled("generator bug: a narrowed document copied into another customer's mapping maps the abstract class with the dropped attribute — fix pending; " + PENDING_FIXTURE)
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
	 * SessionFactory does not start. The fixture is test2's OverrideEnumJoinedBase in src/test/pending.
	 */
	@Test
	@Disabled("generator bug: a narrowed joined subclass copied into another customer's mapping names an enum class that does not exist — fix pending; " + PENDING_FIXTURE)
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
	 * The fixture is test2's OverrideEnumExtended in src/test/pending.
	 */
	@Test
	@Disabled("generator bug: an override keeping an enum another customer narrows away clashes with the Vanilla class's accessor — fix pending; " + PENDING_FIXTURE)
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
