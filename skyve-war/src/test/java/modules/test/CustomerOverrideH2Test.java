package modules.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.ValidationException;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.module.ModuleImpl;
import org.skyve.impl.metadata.repository.LocalDataStoreRepository;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.impl.metadata.view.NoOpViewVisitor;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.ModuleDocument;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.persistence.Persistence;
import org.skyve.util.Binder;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import customers.test.domain.TestCustomerRole;
import customers.test.modules.test.domain.OverrideAddsOverride;
import customers.test.modules.test.domain.OverrideCustomerOnly;
import customers.test.modules.test.domain.OverrideEnumExtendedOverride;
import customers.test.modules.test.domain.OverrideEnumJoinedSubOverride;
import customers.test.modules.test.domain.OverrideExtendedOverride;
import customers.test.modules.test.domain.OverrideJoinedSubOverride;
import customers.test.modules.test.domain.OverrideMappedSubOverride;
import customers.test.modules.test.domain.OverrideNarrowedExtendedOverride;
import customers.test.modules.test.domain.OverrideNarrowedLinkedOverride;
import customers.test.modules.test.domain.OverrideNarrowedOverride;
import customers.test.modules.test.domain.OverrideProxiedOverride;
import customers.test.modules.test.domain.OverrideSingleSubOverride;
import customers.test.modules.test.domain.TestRole;
import modules.admin.Group.GroupExtension;
import modules.admin.ModulesUtil;
import modules.admin.User.UserExtension;
import modules.admin.domain.AdminRole;
import modules.admin.domain.User;
import modules.test.OverrideEnumExtended.OverrideEnumExtendedExtension;
import modules.test.OverrideExtended.OverrideExtendedExtension;
import modules.test.OverrideNarrowedExtended.OverrideNarrowedExtendedExtension;
import modules.test.OverrideProxied.OverrideProxiedExtension;
import modules.test.OverrideProxied.OverrideProxiedFactory;
import modules.test.domain.OverrideAdds;
import modules.test.domain.OverrideEnumExtended;
import modules.test.domain.OverrideEnumExtendedVanilla;
import modules.test.domain.OverrideEnumJoinedBase;
import modules.test.domain.OverrideEnumJoinedSub;
import modules.test.domain.OverrideEnumJoinedSubVanilla;
import modules.test.domain.OverrideExtended;
import modules.test.domain.OverrideJoinedBase;
import modules.test.domain.OverrideJoinedSub;
import modules.test.domain.OverrideLength;
import modules.test.domain.OverrideLinkTarget;
import modules.test.domain.OverrideMappedSub;
import modules.test.domain.OverrideNarrowed;
import modules.test.domain.OverrideNarrowedExtended;
import modules.test.domain.OverrideNarrowedLinked;
import modules.test.domain.OverrideNarrowedLinkedVanilla;
import modules.test.domain.OverrideNarrowedVanilla;
import modules.test.domain.OverrideProxied;
import modules.test.domain.OverrideReferrer;
import modules.test.domain.OverrideRemoved;
import modules.test.domain.OverrideSingleBase;
import modules.test.domain.OverrideSingleSub;
import modules.test.domain.TestDocument;
import util.AbstractH2Test;

/**
 * Exercises the test customer's document overrides against Hibernate: the vanilla mappings and the
 * test and test2 customers' <code>customers/&lt;customer&gt;/modules/orm.hbm.xml</code> share one
 * SessionFactory, so this suite starting at all shows that they load together. The tests compare
 * the test customer with bizhub, the customer H2 tests normally run as, which has no overrides,
 * and with test2, whose overrides combine with the test customer's.
 * <p/>
 * The fixtures are the <code>Override*</code> documents of the test module and their overrides in
 * <code>customers/test/modules/test/</code> and <code>customers/test2/modules/test/</code>.
 */
@SuppressWarnings("static-method")
class CustomerOverrideH2Test extends AbstractH2Test {
	/** The customer whose overrides of the test module are the fixtures. */
	private static final String TEST_CUSTOMER = "test";

	/** The second customer, whose overrides combine with the test customer's. */
	private static final String TEST2_CUSTOMER = "test2";

	/**
	 * Runs the rest of the test as a super user of the customer, in the current transaction, and
	 * returns the customer.
	 */
	private static Customer runAs(String customerName) {
		Customer result = customer(customerName);
		SuperUser user = new SuperUser();
		user.setCustomerName(customerName);
		user.setName(USER);
		user.setId(USER);
		AbstractPersistence.get().setUser(user);
		return result;
	}

	/** Returns the customer's metadata. */
	private static Customer customer(String customerName) {
		Customer result = ProvidedRepositoryFactory.getNullSafe().getCustomer(customerName);
		assertNotNull(result, customerName);
		return result;
	}

	/** Creates a bean of the document for the current user's customer. */
	private static <T extends Bean> T newInstance(ModuleDocument document) throws Exception {
		return document.toDocument().newInstance(CORE.getUser());
	}

	/** Returns the class of the customer's beans of the document. */
	private static Class<?> beanClass(ModuleDocument document, Customer customer) throws Exception {
		return ((DocumentImpl) document.toDocument(customer)).getBeanClass(customer);
	}

	/**
	 * Saves the bean, clears the first level cache, and returns the bean retrieved again from the
	 * data store.
	 */
	private static <T extends PersistentBean> T saveAndRetrieve(ModuleDocument document, T bean) {
		Persistence p = CORE.getPersistence();
		T saved = p.save(bean);
		p.evictAllCached();
		T result = p.retrieve(document, saved.getBizId());
		assertNotNull(result, "retrieve " + document.documentName());
		return result;
	}

	/** The enum constant's code, read whatever class declares the enum. */
	private static String code(Object enumeration) {
		return (enumeration == null) ? null : ((Enumeration) enumeration).toCode();
	}

	@Test
	void anOverrideThatAddsPropertiesUsesItsOverrideClassAndSavesThem() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideCustomerOnly item = newInstance(customers.test.modules.test.domain.TestDocument.OVERRIDE_CUSTOMER_ONLY);
		item.setText("item");
		OverrideCustomerOnly associated = newInstance(customers.test.modules.test.domain.TestDocument.OVERRIDE_CUSTOMER_ONLY);
		associated.setText("associated");
		OverrideAddsOverride bean = newInstance(TestDocument.OVERRIDE_ADDS);
		bean.setText("text");
		bean.setCustomerText("customer");
		bean.setCustomerOnlyItem(associated);
		bean.addCustomerOnlyItemsElement(item);

		OverrideAdds result = saveAndRetrieve(TestDocument.OVERRIDE_ADDS, bean);

		OverrideAddsOverride override = assertInstanceOf(OverrideAddsOverride.class, result);
		assertAll(() -> assertSame(OverrideAddsOverride.class, beanClass(TestDocument.OVERRIDE_ADDS, test)),
					() -> assertEquals("text", override.getText()),
					() -> assertEquals("customer", override.getCustomerText()),
					() -> assertTrue(override.isCustomerTextSet()),
					() -> assertEquals("associated", override.getCustomerOnlyItem().getText()),
					() -> assertEquals(1, override.getCustomerOnlyItems().size()),
					() -> assertEquals("item", override.getCustomerOnlyItems().get(0).getText()));
	}

	/**
	 * The new customer document OverrideCustomerOnly has an owner association to OverrideAdds, whose
	 * inverse ownedItems only the test customer's override declares, so the association is typed as
	 * OverrideAddsOverride and its mutator maintains the inverse.
	 */
	@Test
	void anAssociationFromACustomerDocumentMaintainsTheOverridesInverse() throws Exception {
		runAs(TEST_CUSTOMER);
		OverrideAddsOverride owner = newInstance(TestDocument.OVERRIDE_ADDS);
		owner.setText("owner");
		OverrideCustomerOnly item = newInstance(customers.test.modules.test.domain.TestDocument.OVERRIDE_CUSTOMER_ONLY);
		item.setText("owned");
		item.setOwner(owner);
		int ownedInMemory = owner.getOwnedItems().size();

		OverrideCustomerOnly saved = CORE.getPersistence().save(item);
		CORE.getPersistence().evictAllCached();
		OverrideAddsOverride result = assertInstanceOf(OverrideAddsOverride.class,
														CORE.getPersistence().retrieve(TestDocument.OVERRIDE_ADDS, saved.getOwner().getBizId()));

		assertAll(() -> assertEquals(1, ownedInMemory),
					() -> assertEquals(1, result.getOwnedItems().size()),
					() -> assertEquals("owned", result.getOwnedItems().get(0).getText()));
	}

	@Test
	void aCustomerWithoutTheOverrideUsesTheVanillaClass() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideAdds bean = newInstance(TestDocument.OVERRIDE_ADDS);
		bean.setText("text");

		OverrideAdds result = saveAndRetrieve(TestDocument.OVERRIDE_ADDS, bean);

		assertAll(() -> assertSame(OverrideAdds.class, beanClass(TestDocument.OVERRIDE_ADDS, bizhub)),
					() -> assertSame(OverrideAdds.class, result.getClass()),
					() -> assertEquals("text", result.getText()));
	}

	@Test
	void aCustomerOnlyDocumentResolvesOnlyForItsCustomer() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideCustomerOnly bean = newInstance(customers.test.modules.test.domain.TestDocument.OVERRIDE_CUSTOMER_ONLY);
		bean.setText("customer only");

		OverrideCustomerOnly result = saveAndRetrieve(customers.test.modules.test.domain.TestDocument.OVERRIDE_CUSTOMER_ONLY, bean);

		Customer bizhub = customer(CUSTOMER);
		assertAll(() -> assertSame(OverrideCustomerOnly.class, beanClass(customers.test.modules.test.domain.TestDocument.OVERRIDE_CUSTOMER_ONLY, test)),
					() -> assertEquals("customer only", result.getText()),
					() -> assertThrows(MetaDataException.class,
										() -> customers.test.modules.test.domain.TestDocument.OVERRIDE_CUSTOMER_ONLY.toDocument(bizhub)));
	}

	@Test
	void anOverrideThatDropsAnAttributeUsesItsOverrideClass() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideNarrowed bean = newInstance(TestDocument.OVERRIDE_NARROWED);
		bean.setText("text");

		OverrideNarrowed result = saveAndRetrieve(TestDocument.OVERRIDE_NARROWED, bean);

		assertAll(() -> assertSame(OverrideNarrowedOverride.class, beanClass(TestDocument.OVERRIDE_NARROWED, test)),
					() -> assertSame(OverrideNarrowedOverride.class, bean.getClass()),
					() -> assertSame(OverrideNarrowedOverride.class, result.getClass()),
					() -> assertEquals("text", result.getText()),
					() -> assertFalse(result instanceof OverrideNarrowedVanilla));
	}

	@Test
	void aDroppedAttributeIsStillSavedForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideNarrowedVanilla bean = newInstance(TestDocument.OVERRIDE_NARROWED);
		bean.setText("text");
		bean.setDropped("dropped");

		OverrideNarrowed result = saveAndRetrieve(TestDocument.OVERRIDE_NARROWED, bean);

		OverrideNarrowedVanilla vanilla = assertInstanceOf(OverrideNarrowedVanilla.class, result);
		assertAll(() -> assertSame(OverrideNarrowedVanilla.class, beanClass(TestDocument.OVERRIDE_NARROWED, bizhub)),
					() -> assertEquals("text", vanilla.getText()),
					() -> assertEquals("dropped", vanilla.getDropped()));
	}

	@Test
	void aJoinedSubclassOverrideSavesAndIsFoundThroughItsBase() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideJoinedSubOverride sub = newInstance(TestDocument.OVERRIDE_JOINED_SUB);
		sub.setText("base");
		sub.setSubText("sub");
		sub.setCustomerText("customer");
		// the unchanged override of the base keeps the vanilla class
		OverrideJoinedBase base = newInstance(TestDocument.OVERRIDE_JOINED_BASE);
		base.setText("base only");

		OverrideJoinedSub result = saveAndRetrieve(TestDocument.OVERRIDE_JOINED_SUB, sub);
		OverrideJoinedBase baseResult = saveAndRetrieve(TestDocument.OVERRIDE_JOINED_BASE, base);
		List<OverrideJoinedBase> polymorphic = CORE.getPersistence().newDocumentQuery(TestDocument.OVERRIDE_JOINED_BASE).beanResults();

		OverrideJoinedSubOverride override = assertInstanceOf(OverrideJoinedSubOverride.class, result);
		assertAll(() -> assertSame(OverrideJoinedSubOverride.class, beanClass(TestDocument.OVERRIDE_JOINED_SUB, test)),
					() -> assertSame(OverrideJoinedBase.class, beanClass(TestDocument.OVERRIDE_JOINED_BASE, test)),
					() -> assertEquals("base", override.getText()),
					() -> assertEquals("sub", override.getSubText()),
					() -> assertEquals("customer", override.getCustomerText()),
					() -> assertSame(OverrideJoinedBase.class, baseResult.getClass()),
					() -> assertEquals("base only", baseResult.getText()),
					() -> assertEquals(2, polymorphic.size(), polymorphic.toString()),
					() -> assertTrue(polymorphic.stream().anyMatch(b -> (b instanceof OverrideJoinedSubOverride o) &&
																			"customer".equals(o.getCustomerText())),
										polymorphic.toString()));
	}

	@Test
	void aJoinedSubclassSavesAndIsFoundThroughItsBaseForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideJoinedSub sub = newInstance(TestDocument.OVERRIDE_JOINED_SUB);
		sub.setText("base");
		sub.setSubText("sub");

		OverrideJoinedSub result = saveAndRetrieve(TestDocument.OVERRIDE_JOINED_SUB, sub);
		List<OverrideJoinedBase> polymorphic = CORE.getPersistence().newDocumentQuery(TestDocument.OVERRIDE_JOINED_BASE).beanResults();

		assertAll(() -> assertSame(OverrideJoinedSub.class, beanClass(TestDocument.OVERRIDE_JOINED_SUB, bizhub)),
					() -> assertSame(OverrideJoinedSub.class, result.getClass()),
					() -> assertEquals("sub", result.getSubText()),
					() -> assertEquals(1, polymorphic.size(), polymorphic.toString()),
					() -> assertSame(OverrideJoinedSub.class, polymorphic.get(0).getClass()));
	}

	@Test
	void aSingleTableSubclassOverrideSavesAndIsFoundThroughItsBase() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideSingleSubOverride sub = newInstance(TestDocument.OVERRIDE_SINGLE_SUB);
		sub.setText("base");
		sub.setSubText("sub");
		sub.setCustomerText("customer");

		OverrideSingleSub result = saveAndRetrieve(TestDocument.OVERRIDE_SINGLE_SUB, sub);
		List<OverrideSingleBase> polymorphic = CORE.getPersistence().newDocumentQuery(TestDocument.OVERRIDE_SINGLE_BASE).beanResults();

		OverrideSingleSubOverride override = assertInstanceOf(OverrideSingleSubOverride.class, result);
		assertAll(() -> assertSame(OverrideSingleSubOverride.class, beanClass(TestDocument.OVERRIDE_SINGLE_SUB, test)),
					() -> assertSame(OverrideSingleBase.class, beanClass(TestDocument.OVERRIDE_SINGLE_BASE, test)),
					() -> assertEquals("sub", override.getSubText()),
					() -> assertEquals("customer", override.getCustomerText()),
					() -> assertEquals(1, polymorphic.size(), polymorphic.toString()),
					() -> assertSame(OverrideSingleSubOverride.class, polymorphic.get(0).getClass()));
	}

	@Test
	void aSingleTableSubclassSavesAndIsFoundThroughItsBaseForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideSingleSub sub = newInstance(TestDocument.OVERRIDE_SINGLE_SUB);
		sub.setText("base");
		sub.setSubText("sub");

		OverrideSingleSub result = saveAndRetrieve(TestDocument.OVERRIDE_SINGLE_SUB, sub);
		List<OverrideSingleBase> polymorphic = CORE.getPersistence().newDocumentQuery(TestDocument.OVERRIDE_SINGLE_BASE).beanResults();

		assertAll(() -> assertSame(OverrideSingleSub.class, beanClass(TestDocument.OVERRIDE_SINGLE_SUB, bizhub)),
					() -> assertEquals("sub", result.getSubText()),
					() -> assertEquals(1, polymorphic.size(), polymorphic.toString()),
					() -> assertSame(OverrideSingleSub.class, polymorphic.get(0).getClass()));
	}

	/**
	 * A mapped base has no table or entity, so there is no polymorphic query on it.
	 */
	@Test
	void aSubclassOfAMappedBaseOverrideSaves() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideMappedSubOverride sub = newInstance(TestDocument.OVERRIDE_MAPPED_SUB);
		sub.setText("base");
		sub.setSubText("sub");
		sub.setCustomerText("customer");

		OverrideMappedSub result = saveAndRetrieve(TestDocument.OVERRIDE_MAPPED_SUB, sub);

		OverrideMappedSubOverride override = assertInstanceOf(OverrideMappedSubOverride.class, result);
		Customer bizhub = customer(CUSTOMER);
		assertAll(() -> assertSame(OverrideMappedSubOverride.class, beanClass(TestDocument.OVERRIDE_MAPPED_SUB, test)),
					() -> assertSame(OverrideMappedSub.class, beanClass(TestDocument.OVERRIDE_MAPPED_SUB, bizhub)),
					() -> assertEquals("base", override.getText()),
					() -> assertEquals("sub", override.getSubText()),
					() -> assertEquals("customer", override.getCustomerText()));
	}

	@Test
	void referencesToOverriddenSubclassesResolveToTheirOverrideClasses() throws Exception {
		runAs(TEST_CUSTOMER);
		OverrideJoinedSubOverride joined = newInstance(TestDocument.OVERRIDE_JOINED_SUB);
		joined.setText("joined");
		joined.setCustomerText("joined customer");
		OverrideSingleSubOverride single = newInstance(TestDocument.OVERRIDE_SINGLE_SUB);
		single.setText("single");
		single.setCustomerText("single customer");
		OverrideMappedSubOverride mapped = newInstance(TestDocument.OVERRIDE_MAPPED_SUB);
		mapped.setText("mapped");
		mapped.setCustomerText("mapped customer");
		OverrideReferrer referrer = newInstance(TestDocument.OVERRIDE_REFERRER);
		referrer.setText("referrer");
		referrer.setJoinedSub(joined);
		referrer.setSingleSub(single);
		referrer.setMappedSub(mapped);

		OverrideReferrer result = saveAndRetrieve(TestDocument.OVERRIDE_REFERRER, referrer);

		assertAll(() -> assertSame(OverrideReferrer.class, result.getClass()),
					() -> assertEquals("joined customer", assertInstanceOf(OverrideJoinedSubOverride.class, result.getJoinedSub()).getCustomerText()),
					() -> assertEquals("single customer", assertInstanceOf(OverrideSingleSubOverride.class, result.getSingleSub()).getCustomerText()),
					() -> assertEquals("mapped customer", assertInstanceOf(OverrideMappedSubOverride.class, result.getMappedSub()).getCustomerText()));
	}

	@Test
	void referencesToSubclassesResolveForOtherCustomers() throws Exception {
		runAs(CUSTOMER);
		OverrideJoinedSub joined = newInstance(TestDocument.OVERRIDE_JOINED_SUB);
		joined.setText("joined");
		OverrideReferrer referrer = newInstance(TestDocument.OVERRIDE_REFERRER);
		referrer.setText("referrer");
		referrer.setJoinedSub(joined);

		OverrideReferrer result = saveAndRetrieve(TestDocument.OVERRIDE_REFERRER, referrer);

		OverrideJoinedSub resultJoined = result.getJoinedSub();
		assertNotNull(resultJoined);
		assertAll(() -> assertEquals("joined", resultJoined.getText()),
					() -> assertFalse(resultJoined instanceof OverrideJoinedSubOverride));
	}

	@Test
	void anOverrideOfADocumentWithAnExtensionExtendsTheExtension() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideExtendedOverride bean = newInstance(TestDocument.OVERRIDE_EXTENDED);
		bean.setText("text");
		bean.setCustomerText("customer");

		OverrideExtended result = saveAndRetrieve(TestDocument.OVERRIDE_EXTENDED, bean);

		OverrideExtendedOverride override = assertInstanceOf(OverrideExtendedOverride.class, result);
		assertAll(() -> assertSame(OverrideExtendedOverride.class, beanClass(TestDocument.OVERRIDE_EXTENDED, test)),
					() -> assertEquals("customer", override.getCustomerText()),
					() -> assertEquals("Extended text", override.describe()));
	}

	@Test
	void aDocumentWithAnExtensionUsesItForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideExtendedExtension bean = newInstance(TestDocument.OVERRIDE_EXTENDED);
		bean.setText("text");

		OverrideExtended result = saveAndRetrieve(TestDocument.OVERRIDE_EXTENDED, bean);

		assertAll(() -> assertSame(OverrideExtendedExtension.class, beanClass(TestDocument.OVERRIDE_EXTENDED, bizhub)),
					() -> assertSame(OverrideExtendedExtension.class, result.getClass()),
					() -> assertEquals("Extended text", ((OverrideExtendedExtension) result).describe()));
	}

	@Test
	void aNarrowingOverrideOfADocumentWithAnExtensionExtendsTheExtension() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideNarrowedExtendedOverride bean = newInstance(TestDocument.OVERRIDE_NARROWED_EXTENDED);
		bean.setText("text");

		OverrideNarrowedExtended result = saveAndRetrieve(TestDocument.OVERRIDE_NARROWED_EXTENDED, bean);

		OverrideNarrowedExtendedOverride override = assertInstanceOf(OverrideNarrowedExtendedOverride.class, result);
		assertAll(() -> assertSame(OverrideNarrowedExtendedOverride.class, beanClass(TestDocument.OVERRIDE_NARROWED_EXTENDED, test)),
					() -> assertEquals("text", override.getText()),
					() -> assertEquals("Narrowed extended text", override.describe()));
	}

	@Test
	void aNarrowedDocumentsExtensionKeepsTheDroppedAttributeForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideNarrowedExtendedExtension bean = newInstance(TestDocument.OVERRIDE_NARROWED_EXTENDED);
		bean.setText("text");
		bean.setDropped("dropped");

		OverrideNarrowedExtended result = saveAndRetrieve(TestDocument.OVERRIDE_NARROWED_EXTENDED, bean);

		OverrideNarrowedExtendedExtension extension = assertInstanceOf(OverrideNarrowedExtendedExtension.class, result);
		assertAll(() -> assertSame(OverrideNarrowedExtendedExtension.class, beanClass(TestDocument.OVERRIDE_NARROWED_EXTENDED, bizhub)),
					() -> assertSame(OverrideNarrowedExtendedExtension.class, result.getClass()),
					() -> assertEquals("dropped", extension.getDropped()),
					() -> assertEquals("Narrowed extended text", extension.describe()));
	}

	@Test
	void aLongerLengthWidensTheSharedColumn() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		String longText = "x".repeat(200);
		OverrideLength testBean = newInstance(TestDocument.OVERRIDE_LENGTH);
		testBean.setText(longText);
		OverrideLength testResult = saveAndRetrieve(TestDocument.OVERRIDE_LENGTH, testBean);

		Customer bizhub = runAs(CUSTOMER);
		String vanillaText = "y".repeat(50);
		OverrideLength bizhubBean = newInstance(TestDocument.OVERRIDE_LENGTH);
		bizhubBean.setText(vanillaText);
		OverrideLength bizhubResult = saveAndRetrieve(TestDocument.OVERRIDE_LENGTH, bizhubBean);
		OverrideLength tooLong = newInstance(TestDocument.OVERRIDE_LENGTH);
		tooLong.setText(vanillaText + 'y');

		Persistence p = CORE.getPersistence();
		Persistent persistent = TestDocument.OVERRIDE_LENGTH.toDocument().getPersistent();
		String tableName = (persistent == null) ? null : persistent.getPersistentIdentifier();
		Number columnLength = p.newSQL("select CHARACTER_MAXIMUM_LENGTH from INFORMATION_SCHEMA.COLUMNS " +
											"where upper(TABLE_NAME) = upper(:tableName) and upper(COLUMN_NAME) = upper(:columnName)")
								.putParameter("tableName", tableName, false)
								.putParameter("columnName", OverrideLength.textPropertyName, false)
								.scalarResult(Number.class);
		assertAll(() -> assertSame(OverrideLength.class, beanClass(TestDocument.OVERRIDE_LENGTH, test)),
					() -> assertSame(OverrideLength.class, beanClass(TestDocument.OVERRIDE_LENGTH, bizhub)),
					() -> assertEquals(longText, testResult.getText()),
					() -> assertEquals(vanillaText, bizhubResult.getText()),
					() -> assertEquals(200, (columnLength == null) ? 0 : columnLength.intValue()),
					// the column is shared, but bizhub's metadata still has the shorter length
					() -> assertThrows(ValidationException.class, () -> p.save(tooLong)));
	}

	@Test
	void aCustomerRoleExpandsToItsModuleRolesAtSignIn() {
		runAs(TEST_CUSTOMER);
		long suffix = System.nanoTime();
		String userName = "test.manager." + suffix;
		GroupExtension group = ModulesUtil.configureGroup("Test managers " + suffix, TestCustomerRole.TEST_MANAGER);
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setUserName(userName);
		user.setInactive(Boolean.FALSE);
		user.getRoles().clear();
		user.getGroups().clear();
		user.addGroupsElement(group);
		Persistence p = CORE.getPersistence();
		p.save(user);
		// sign-in reads the user's roles through its own connection
		p.commit(false);
		p.begin();

		UserImpl signedIn = new LocalDataStoreRepository().retrieveUser(TEST_CUSTOMER + '/' + userName);

		assertNotNull(signedIn);
		assertAll(() -> assertEquals(TEST_CUSTOMER, signedIn.getCustomerName()),
					() -> assertTrue(signedIn.isInRole(AdminRole.BASIC_USER)),
					() -> assertTrue(signedIn.isInRole(TestRole.TEST_ROLE)),
					() -> assertFalse(signedIn.isInRole(AdminRole.SECURITY_ADMINISTRATOR)));
	}

	@Test
	void aDocumentTheModuleOverrideLeavesOutDoesNotResolveForTheCustomer() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		Customer bizhub = customer(CUSTOMER);

		assertAll(() -> assertTrue(Arrays.stream(customers.test.modules.test.domain.TestDocument.values())
												.noneMatch(d -> OverrideRemoved.DOCUMENT_NAME.equals(d.documentName()))),
					() -> assertThrows(MetaDataException.class, () -> TestDocument.OVERRIDE_REMOVED.toDocument(test)),
					() -> assertSame(OverrideRemoved.class, beanClass(TestDocument.OVERRIDE_REMOVED, bizhub)));
		runAs(CUSTOMER);
		assertSame(OverrideRemoved.class, newInstance(TestDocument.OVERRIDE_REMOVED).getClass());
	}

	@Test
	void aSubclassOfAMappedDocumentWithAnExtensionUsesItForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideProxiedExtension bean = newInstance(TestDocument.OVERRIDE_PROXIED);
		bean.setText("text");
		bean.setSubText("sub");

		OverrideProxied result = saveAndRetrieve(TestDocument.OVERRIDE_PROXIED, bean);

		assertAll(() -> assertSame(OverrideProxiedExtension.class, beanClass(TestDocument.OVERRIDE_PROXIED, bizhub)),
					() -> assertSame(OverrideProxiedExtension.class, result.getClass()),
					() -> assertEquals("sub", result.getSubText()),
					() -> assertEquals("Proxied text", ((OverrideProxiedExtension) result).describe()),
					() -> assertEquals(OverrideProxiedFactory.CRUD_TEXT,
										new DataBuilder().fixture(FixtureType.crud).<OverrideProxied>build(TestDocument.OVERRIDE_PROXIED).getText()));
	}

	@Test
	void aNarrowingOverrideWithAnAssociationSavesTheAssociation() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideLinkTarget target = newInstance(TestDocument.OVERRIDE_LINK_TARGET);
		target.setText("target");
		OverrideNarrowedLinkedOverride bean = newInstance(TestDocument.OVERRIDE_NARROWED_LINKED);
		bean.setText("text");
		bean.setTarget(target);

		OverrideNarrowedLinked result = saveAndRetrieve(TestDocument.OVERRIDE_NARROWED_LINKED, bean);

		OverrideLinkTarget resultTarget = result.getTarget();
		assertNotNull(resultTarget);
		assertAll(() -> assertSame(OverrideNarrowedLinkedOverride.class, beanClass(TestDocument.OVERRIDE_NARROWED_LINKED, test)),
					() -> assertSame(OverrideNarrowedLinkedOverride.class, result.getClass()),
					() -> assertEquals("target", resultTarget.getText()));
	}

	@Test
	void aNarrowedDocumentWithAnAssociationKeepsTheDroppedAttributeForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideLinkTarget target = newInstance(TestDocument.OVERRIDE_LINK_TARGET);
		target.setText("target");
		OverrideNarrowedLinkedVanilla bean = newInstance(TestDocument.OVERRIDE_NARROWED_LINKED);
		bean.setText("text");
		bean.setDropped("dropped");
		bean.setTarget(target);

		OverrideNarrowedLinked result = saveAndRetrieve(TestDocument.OVERRIDE_NARROWED_LINKED, bean);

		OverrideNarrowedLinkedVanilla vanilla = assertInstanceOf(OverrideNarrowedLinkedVanilla.class, result);
		OverrideLinkTarget resultTarget = vanilla.getTarget();
		assertNotNull(resultTarget);
		assertAll(() -> assertSame(OverrideNarrowedLinkedVanilla.class, beanClass(TestDocument.OVERRIDE_NARROWED_LINKED, bizhub)),
					() -> assertEquals("dropped", vanilla.getDropped()),
					() -> assertEquals("target", resultTarget.getText()));
	}

	@Test
	void aNarrowingOverrideOfAJoinedSubclassSavesAndIsFoundThroughItsBase() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideEnumJoinedSubOverride bean = newInstance(TestDocument.OVERRIDE_ENUM_JOINED_SUB);
		bean.setText("base");
		bean.setSubText("sub");

		OverrideEnumJoinedSub result = saveAndRetrieve(TestDocument.OVERRIDE_ENUM_JOINED_SUB, bean);
		List<OverrideEnumJoinedBase> polymorphic = CORE.getPersistence().newDocumentQuery(TestDocument.OVERRIDE_ENUM_JOINED_BASE).beanResults();

		assertAll(() -> assertSame(OverrideEnumJoinedSubOverride.class, beanClass(TestDocument.OVERRIDE_ENUM_JOINED_SUB, test)),
					() -> assertSame(OverrideEnumJoinedSubOverride.class, result.getClass()),
					() -> assertEquals("sub", result.getSubText()),
					() -> assertEquals(1, polymorphic.size(), polymorphic.toString()),
					() -> assertSame(OverrideEnumJoinedSubOverride.class, polymorphic.get(0).getClass()));
	}

	@Test
	void aNarrowingOverrideOfADocumentWithAnEnumAndAnExtensionExtendsTheExtension() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideEnumExtendedOverride bean = newInstance(TestDocument.OVERRIDE_ENUM_EXTENDED);
		bean.setText("text");

		OverrideEnumExtended result = saveAndRetrieve(TestDocument.OVERRIDE_ENUM_EXTENDED, bean);

		OverrideEnumExtendedOverride override = assertInstanceOf(OverrideEnumExtendedOverride.class, result);
		assertAll(() -> assertSame(OverrideEnumExtendedOverride.class, beanClass(TestDocument.OVERRIDE_ENUM_EXTENDED, test)),
					() -> assertEquals("text", override.getText()),
					() -> assertEquals("Enum extended text", override.describe()));
	}

	@Test
	void aNarrowedDocumentsExtensionKeepsTheDroppedEnumForOtherCustomers() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		OverrideEnumExtendedExtension bean = newInstance(TestDocument.OVERRIDE_ENUM_EXTENDED);
		bean.setText("text");
		bean.setStatus(OverrideEnumExtendedVanilla.Status.closed);

		OverrideEnumExtended result = saveAndRetrieve(TestDocument.OVERRIDE_ENUM_EXTENDED, bean);

		OverrideEnumExtendedExtension extension = assertInstanceOf(OverrideEnumExtendedExtension.class, result);
		assertAll(() -> assertSame(OverrideEnumExtendedExtension.class, beanClass(TestDocument.OVERRIDE_ENUM_EXTENDED, bizhub)),
					() -> assertSame(OverrideEnumExtendedVanilla.Status.closed, extension.getStatus()),
					() -> assertEquals("Enum extended text", extension.describe()));
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
	 * UserExtension. The test customer's override adds customerText, so the customer mapping maps the
	 * generated OverrideProxiedOverride, which extends OverrideProxiedExtension.
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
	 * The test customer's OverrideProxied bean class is OverrideProxiedOverride, which extends the
	 * OverrideProxiedExtension that OverrideProxiedFactory.crudInstance() returns, so DataBuilder uses
	 * the vanilla factory for the test customer too, as it would UserFactory for a customer that
	 * overrides admin.User.
	 */
	@Test
	void aDataFactoryAppliesToACustomersOverrideClass() {
		runAs(TEST_CUSTOMER);
		OverrideProxied bean = new DataBuilder().fixture(FixtureType.crud).build(TestDocument.OVERRIDE_PROXIED);

		assertAll(() -> assertSame(OverrideProxiedOverride.class, bean.getClass()),
					() -> assertEquals(OverrideProxiedFactory.CRUD_TEXT, bean.getText()));
	}

	/**
	 * The test customer's override of the joined subclass OverrideEnumJoinedSub leaves out status, so
	 * OverrideEnumJoinedSub is abstract and other customers get OverrideEnumJoinedSubVanilla, which
	 * declares status and which the vanilla mapping maps.
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
	 * OverrideNarrowedLinked, mapping OverrideNarrowedLinkedVanilla.
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
	 * OverrideEnumJoinedBase without changes, so test2's mapping copies the hierarchy, mapping
	 * OverrideEnumJoinedSubVanilla and its status enum.
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
	 * status. The test2 customer's override keeps status, and its generated
	 * OverrideEnumExtendedOverride, which extends the extension, inherits status.
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

	/**
	 * Asserts that the status enum of the customer's version of the document is the given enum type
	 * and that its constant domain values come from it.
	 */
	private static void assertStatusEnum(Customer customer, ModuleDocument document, Class<?> expected) throws Exception {
		Document customerDocument = document.toDocument(customer);
		Attribute status = customerDocument.getAttribute("status");
		assertNotNull(status, document.toString());
		List<DomainValue> values = customer.getConstantDomainValues(null, customerDocument.getOwningModuleName(), customerDocument.getName(), status);
		List<String> codes = values.stream().map(DomainValue::getCode).toList();
		assertAll(() -> assertSame(expected, status.getImplementingType(), document.toString()),
					() -> assertEquals(List.of("open", "closed"), codes, document.toString()));
	}

	/**
	 * The test customer's overrides of OverrideEnumJoinedSub and OverrideEnumExtended leave out the
	 * status enum, so only their <code>&lt;Document&gt;Vanilla</code> classes declare it, for the
	 * customers with the vanilla documents and for test2, whose override of OverrideEnumExtended keeps
	 * status and inherits it through the hand-written extension.
	 */
	@Test
	void aNarrowedAwayEnumResolvesFromTheVanillaClass() throws Exception {
		Customer bizhub = runAs(CUSTOMER);
		assertStatusEnum(bizhub, TestDocument.OVERRIDE_ENUM_JOINED_SUB, OverrideEnumJoinedSubVanilla.Status.class);
		assertStatusEnum(bizhub, TestDocument.OVERRIDE_ENUM_EXTENDED, OverrideEnumExtendedVanilla.Status.class);
		Customer test2 = runAs(TEST2_CUSTOMER);
		assertStatusEnum(test2, TestDocument.OVERRIDE_ENUM_JOINED_SUB, OverrideEnumJoinedSubVanilla.Status.class);
		assertStatusEnum(test2, TestDocument.OVERRIDE_ENUM_EXTENDED, OverrideEnumExtendedVanilla.Status.class);
	}
}
