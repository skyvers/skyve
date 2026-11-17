package modules.test;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.repository.LocalDataStoreRepository;
import org.skyve.impl.metadata.user.UserImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.persistence.Persistence;
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
import modules.test.domain.OverrideExtended;
import modules.test.domain.OverrideJoinedBase;
import modules.test.domain.OverrideJoinedSub;
import modules.test.domain.OverrideLength;
import modules.test.domain.OverrideLinkTarget;
import modules.test.domain.OverrideMappedSub;
import modules.test.domain.OverrideNarrowed;
import modules.test.domain.OverrideNarrowedExtended;
import modules.test.domain.OverrideNarrowedExtendedVanilla;
import modules.test.domain.OverrideNarrowedLinked;
import modules.test.domain.OverrideNarrowedLinkedVanilla;
import modules.test.domain.OverrideNarrowedVanilla;
import modules.test.domain.OverrideProxied;
import modules.test.domain.OverrideReferrer;
import modules.test.domain.OverrideRemoved;
import modules.test.domain.OverrideSingleBase;
import modules.test.domain.OverrideSingleSub;
import modules.test.domain.TestDocument;
/**
 * Exercises the test customer's document overrides against Hibernate: the vanilla mappings and the
 * test customer's <code>customers/test/modules/orm.hbm.xml</code> share one SessionFactory, so this
 * suite starting at all shows that they load together. The tests compare the test customer with
 * bizhub, the customer H2 tests normally run as, which has no overrides.
 * <p/>
 * The fixtures are the <code>Override*</code> documents of the test module and their overrides in
 * <code>customers/test/modules/test/</code>.
 */
@SuppressWarnings("static-method")
class CustomerOverrideH2Test extends AbstractCustomerOverrideH2Test {
	@Test
	void anOverrideThatAddsPropertiesUsesItsOverrideClassAndSavesThem() throws Exception {
		Customer test = runAs(TEST_CUSTOMER);
		OverrideCustomerOnly item = newInstance(customers.test.modules.test.domain.TestDocument.OVERRIDE_CUSTOMER_ONLY);
		item.setText("item");
		OverrideAddsOverride bean = newInstance(TestDocument.OVERRIDE_ADDS);
		bean.setText("text");
		bean.setCustomerText("customer");
		bean.addCustomerOnlyItemsElement(item);

		OverrideAdds result = saveAndRetrieve(TestDocument.OVERRIDE_ADDS, bean);

		OverrideAddsOverride override = assertInstanceOf(OverrideAddsOverride.class, result);
		assertAll(() -> assertSame(OverrideAddsOverride.class, beanClass(TestDocument.OVERRIDE_ADDS, test)),
					() -> assertEquals("text", override.getText()),
					() -> assertEquals("customer", override.getCustomerText()),
					() -> assertTrue(override.isCustomerTextSet()),
					() -> assertEquals(1, override.getCustomerOnlyItems().size()),
					() -> assertEquals("item", override.getCustomerOnlyItems().get(0).getText()));
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
					// see CustomerOverridePendingH2Test for the exception type
					() -> assertThrows(RuntimeException.class,
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
					() -> assertTrue(extension instanceof OverrideNarrowedExtendedVanilla),
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

		Number columnLength = CORE.getPersistence()
									.newSQL("select CHARACTER_MAXIMUM_LENGTH from INFORMATION_SCHEMA.COLUMNS " +
												"where upper(TABLE_NAME) = 'TEST_OVERRIDELENGTH' and upper(COLUMN_NAME) = 'TEXT'")
									.scalarResult(Number.class);
		assertAll(() -> assertSame(OverrideLength.class, beanClass(TestDocument.OVERRIDE_LENGTH, test)),
					() -> assertSame(OverrideLength.class, beanClass(TestDocument.OVERRIDE_LENGTH, bizhub)),
					() -> assertEquals(longText, testResult.getText()),
					() -> assertEquals(vanillaText, bizhubResult.getText()),
					() -> assertEquals(200, (columnLength == null) ? 0 : columnLength.intValue()),
					// the column is shared, but bizhub's metadata still has the shorter length
					() -> assertThrows(ValidationException.class, () -> CORE.getPersistence().save(tooLong)));
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
					// see CustomerOverridePendingH2Test for the exception type
					() -> assertThrows(RuntimeException.class, () -> TestDocument.OVERRIDE_REMOVED.toDocument(test)),
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
}
