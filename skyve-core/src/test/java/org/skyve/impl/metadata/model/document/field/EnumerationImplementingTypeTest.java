package org.skyve.impl.metadata.model.document.field;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.ModuleMemberTestSupport;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.user.User;

import net.bytebuddy.ByteBuddy;
import net.bytebuddy.description.modifier.TypeManifestation;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.dynamic.loading.ClassLoadingStrategy;

/**
 * Tests that {@link Enumeration#getImplementingType()} loads a generated enum as Java resolves an
 * inherited nested type: from the bean class of the document that declares the enumeration, for the
 * current user's customer, and up its superclasses. The documents are in the module utmod, which
 * the utcust customer overrides:
 * <ul>
 * 	<li>UtEnumDoc is abstract, as utcust's override leaves out its status and kept attributes, so
 * 		UtEnumDocVanilla declares them for other customers. The override adds added and declares
 * 		kept itself, in UtEnumDocOverride.</li>
 * 	<li>UtExtDoc is narrowed the same way and has a hand-written extension, so utcust's
 * 		UtExtDocOverride extends UtExtDocExtension, which extends UtExtDocVanilla.</li>
 * 	<li>UtOnlyDoc is a new document only utcust has.</li>
 * </ul>
 * The bean classes are made at run time, and a context class loader serves them and this test's
 * enums by the names of the generated classes, as classes in the <code>modules</code> and
 * <code>customers</code> packages of the test classes would be found as metadata by other tests.
 */
@SuppressWarnings("static-method")
class EnumerationImplementingTypeTest {
	private static final String MODULE = "utmod";
	private static final String OVERRIDING_CUSTOMER = "utcust";

	/** Stands in for <code>modules.utmod.domain.UtEnumDoc$Kind</code>. */
	private enum VanillaDomainClassKind {
		one
	}

	/** Stands in for <code>modules.utmod.domain.UtEnumDocVanilla$Status</code>. */
	private enum VanillaClassStatus {
		open
	}

	/** Stands in for <code>modules.utmod.domain.UtEnumDocVanilla$Kept</code>. */
	private enum VanillaClassKept {
		open
	}

	/** Stands in for <code>customers.utcust.modules.utmod.domain.UtEnumDocOverride$Added</code>. */
	private enum OverrideClassAdded {
		open
	}

	/** Stands in for <code>customers.utcust.modules.utmod.domain.UtEnumDocOverride$Kept</code>. */
	private enum OverrideClassKept {
		open
	}

	/** Stands in for <code>modules.utmod.domain.UtExtDocVanilla$Status</code>. */
	private enum ExtendedVanillaClassStatus {
		open
	}

	/** Stands in for <code>customers.utcust.modules.utmod.domain.UtOnlyDoc$Kind</code>. */
	private enum CustomerDocumentKind {
		one
	}

	/** The classes by the names Skyve looks them up by. */
	private static final Map<String, Class<?>> CLASSES = new HashMap<>();
	static {
		Class<?> enumDoc = makeAbstract("modules.utmod.domain.UtEnumDoc", Object.class);
		put(enumDoc);
		put(make("modules.utmod.domain.UtEnumDocVanilla", enumDoc));
		put(make("customers.utcust.modules.utmod.domain.UtEnumDocOverride", enumDoc));

		Class<?> extDoc = makeAbstract("modules.utmod.domain.UtExtDoc", Object.class);
		put(extDoc);
		Class<?> extDocVanilla = makeAbstract("modules.utmod.domain.UtExtDocVanilla", extDoc);
		put(extDocVanilla);
		Class<?> extDocExtension = make("modules.utmod.UtExtDoc.UtExtDocExtension", extDocVanilla);
		put(extDocExtension);
		put(make("customers.utcust.modules.utmod.domain.UtExtDocOverride", extDocExtension));

		put(make("customers.utcust.modules.utmod.domain.UtOnlyDoc", Object.class));

		CLASSES.put("modules.utmod.domain.UtEnumDoc$Kind", VanillaDomainClassKind.class);
		CLASSES.put("modules.utmod.domain.UtEnumDocVanilla$Status", VanillaClassStatus.class);
		CLASSES.put("modules.utmod.domain.UtEnumDocVanilla$Kept", VanillaClassKept.class);
		CLASSES.put("customers.utcust.modules.utmod.domain.UtEnumDocOverride$Added", OverrideClassAdded.class);
		CLASSES.put("customers.utcust.modules.utmod.domain.UtEnumDocOverride$Kept", OverrideClassKept.class);
		CLASSES.put("modules.utmod.domain.UtExtDocVanilla$Status", ExtendedVanillaClassStatus.class);
		CLASSES.put("customers.utcust.modules.utmod.domain.UtOnlyDoc$Kind", CustomerDocumentKind.class);
	}

	/** The documents utcust overrides or adds. */
	private static final Set<String> OVERRIDDEN = Set.of("UtEnumDoc", "UtExtDoc", "UtOnlyDoc");

	/**
	 * Makes a class of the name that extends the superclass.
	 */
	private static Class<?> make(String name, Class<?> superclass) {
		return make(name, superclass, TypeManifestation.PLAIN);
	}

	/**
	 * Makes an abstract class of the name that extends the superclass, as a narrowed document's
	 * domain class, and a generated class with a hand-written extension, are abstract.
	 */
	private static Class<?> makeAbstract(String name, Class<?> superclass) {
		return make(name, superclass, TypeManifestation.ABSTRACT);
	}

	@SuppressWarnings("resource") // the made and loaded types only hold the class bytes in memory
	private static Class<?> make(String name, Class<?> superclass, TypeManifestation manifestation) {
		ClassLoader parent = (superclass.getClassLoader() == null) ?
								EnumerationImplementingTypeTest.class.getClassLoader() :
								superclass.getClassLoader();
		return new ByteBuddy().subclass(superclass)
								.name(name)
								.modifiers(Visibility.PUBLIC, manifestation)
								.make()
								.load(parent, ClassLoadingStrategy.Default.WRAPPER)
								.getLoaded();
	}

	private static void put(Class<?> type) {
		CLASSES.put(type.getName(), type);
	}

	private ProvidedRepository previousRepository;
	private ClassLoader previousClassLoader;
	private final Map<String, DocumentImpl> documents = new HashMap<>();

	@BeforeEach
	void setUp() throws Exception {
		ModuleMemberTestSupport.clearPersistenceThreadLocal();

		previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.vtable(any(), anyString())).thenAnswer(invocation -> {
			String customerName = invocation.getArgument(0);
			String key = invocation.getArgument(1);
			String[] parts = key.split("/");
			if (parts.length == 4) { // modules/<m>/<Doc>/<Doc>Extension
				return "UtExtDoc".equals(parts[2]) ? key : null;
			}
			if (OVERRIDING_CUSTOMER.equals(customerName) && OVERRIDDEN.contains(parts[2])) {
				return "customers/" + customerName + '/' + key;
			}
			return "UtOnlyDoc".equals(parts[2]) ? null : key;
		});
		Module module = mock(Module.class);
		when(module.getDocument(any(), anyString())).thenAnswer(invocation -> documents.get(invocation.getArgument(1)));
		when(repository.getModule(any(), anyString())).thenReturn(module);
		ProvidedRepositoryFactory.set(repository);

		Thread thread = Thread.currentThread();
		previousClassLoader = thread.getContextClassLoader();
		thread.setContextClassLoader(new ClassLoader(previousClassLoader) {
			@Override
			protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
				Class<?> result = CLASSES.get(name);
				if (result != null) {
					return result;
				}
				if (name.startsWith("modules.") || name.startsWith("customers.")) {
					throw new ClassNotFoundException(name);
				}
				return super.loadClass(name, resolve);
			}
		});
	}

	@AfterEach
	void tearDown() throws Exception {
		ModuleMemberTestSupport.clearPersistenceThreadLocal();
		Thread.currentThread().setContextClassLoader(previousClassLoader);
		ProvidedRepositoryFactory.set(previousRepository);
	}

	/**
	 * Runs the rest of the test as a user of the customer.
	 */
	private static void runAs(String customerName) {
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn(customerName);
		User user = mock(User.class);
		when(user.getCustomerName()).thenReturn(customerName);
		when(user.getCustomer()).thenReturn(customer);
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
	}

	/**
	 * An enumeration attribute of the document in the module utmod.
	 */
	private Enumeration enumeration(String documentName, String name) {
		DocumentImpl document = documents.computeIfAbsent(documentName, k -> {
			DocumentImpl result = new DocumentImpl();
			result.setName(k);
			result.setOwningModuleName(MODULE);
			return result;
		});
		Enumeration result = new Enumeration();
		result.setName(name);
		result.setOwningDocument(document);
		document.putAttribute(result);
		return result;
	}

	@Test
	void anEnumOfTheVanillaDomainClassLoadsFromIt() {
		Enumeration kind = enumeration("UtEnumDoc", "kind");
		Class<?> withoutCustomer = kind.getImplementingType();
		runAs(OVERRIDING_CUSTOMER);

		assertAll(() -> assertSame(VanillaDomainClassKind.class, withoutCustomer),
					() -> assertSame(VanillaDomainClassKind.class, kind.getImplementingType()));
	}

	@Test
	void aNarrowedAwayEnumLoadsFromTheVanillaClass() {
		Enumeration status = enumeration("UtEnumDoc", "status");
		Class<?> withoutCustomer = status.getImplementingType();
		runAs("other");

		assertAll(() -> assertSame(VanillaClassStatus.class, withoutCustomer),
					() -> assertSame(VanillaClassStatus.class, status.getImplementingType()));
	}

	/**
	 * The override's class extends the vanilla extension, which extends the vanilla class, so the
	 * override finds the narrowed-away enum there.
	 */
	@Test
	void aNarrowedAwayEnumLoadsThroughTheVanillaExtensionForTheOverride() {
		Enumeration status = enumeration("UtExtDoc", "status");
		runAs(OVERRIDING_CUSTOMER);

		assertSame(ExtendedVanillaClassStatus.class, status.getImplementingType());
	}

	@Test
	void anEnumAnOverrideAddsLoadsFromTheCustomersOverrideClass() {
		runAs(OVERRIDING_CUSTOMER);

		assertSame(OverrideClassAdded.class, enumeration("UtEnumDoc", "added").getImplementingType());
	}

	/**
	 * One enumeration resolves for each customer, so what it loads for one customer is not what it
	 * gives another.
	 */
	@Test
	void anEnumTheOverrideAndTheVanillaClassDeclareLoadsForEachCustomer() {
		Enumeration kept = enumeration("UtEnumDoc", "kept");
		runAs(OVERRIDING_CUSTOMER);
		Class<?> overriding = kept.getImplementingType();
		runAs("other");
		Class<?> other = kept.getImplementingType();
		runAs(OVERRIDING_CUSTOMER);

		assertAll(() -> assertSame(OverrideClassKept.class, overriding),
					() -> assertSame(VanillaClassKept.class, other),
					() -> assertSame(OverrideClassKept.class, kept.getImplementingType()));
	}

	/** A new customer document's class is in the customer's package. */
	@Test
	void anEnumOfANewCustomerDocumentLoadsFromItsClass() {
		runAs(OVERRIDING_CUSTOMER);

		assertSame(CustomerDocumentKind.class, enumeration("UtOnlyDoc", "kind").getImplementingType());
	}

	/** A reference to another document's enumeration loads it through that document's bean class. */
	@Test
	void anEnumReferencedFromAnotherDocumentLoadsFromThatDocumentsClasses() {
		enumeration("UtEnumDoc", "kept");
		Enumeration reference = enumeration("UtReferrer", "keptReference");
		reference.setModuleRef(MODULE);
		reference.setDocumentRef("UtEnumDoc");
		reference.setAttributeRef("kept");
		runAs(OVERRIDING_CUSTOMER);
		Class<?> overriding = reference.getImplementingType();
		runAs("other");

		assertAll(() -> assertSame(OverrideClassKept.class, overriding),
					() -> assertSame(VanillaClassKept.class, reference.getImplementingType()));
	}

	/**
	 * A vanilla document's reference to an enumeration that the customer's override leaves out
	 * loads it from the vanilla classes, which the vanilla document's class uses.
	 */
	@Test
	void aNarrowedAwayEnumReferencedFromAnotherDocumentLoadsFromTheVanillaClassForTheOverridingCustomer() {
		enumeration("UtEnumDoc", "status");
		Enumeration reference = enumeration("UtReferrer", "statusReference");
		reference.setModuleRef(MODULE);
		reference.setDocumentRef("UtEnumDoc");
		reference.setAttributeRef("status");
		runAs(OVERRIDING_CUSTOMER);

		assertSame(VanillaClassStatus.class, reference.getImplementingType());
	}

	@Test
	void anUndeclaredEnumThrowsAnEnumClassLoadingFailure() {
		runAs(OVERRIDING_CUSTOMER);
		Enumeration missing = enumeration("UtEnumDoc", "missing");

		MetaDataException e = assertThrows(MetaDataException.class, missing::getImplementingType);

		assertAll(() -> assertEquals("A problem was encountered loading enum customers.utcust.modules.utmod.domain.UtEnumDocOverride$Missing", e.getMessage()),
					() -> assertTrue(Enumeration.isEnumClassLoadingFailure(e), e::toString));
	}

	/** The domain classes are not compiled yet when the domain is first generated. */
	@Test
	void anEnumOfADocumentWithoutABeanClassThrowsAnEnumClassLoadingFailure() {
		Enumeration kind = enumeration("UtUncompiled", "kind");

		MetaDataException e = assertThrows(MetaDataException.class, kind::getImplementingType);

		assertAll(() -> assertEquals("A problem was encountered loading enum Kind of document utmod.UtUncompiled", e.getMessage()),
					() -> assertTrue(Enumeration.isEnumClassLoadingFailure(e), e::toString));
	}
}
