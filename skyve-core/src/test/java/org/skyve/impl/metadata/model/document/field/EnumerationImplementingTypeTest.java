package org.skyve.impl.metadata.model.document.field;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

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
import net.bytebuddy.dynamic.scaffold.TypeValidation;
import net.bytebuddy.description.modifier.TypeManifestation;
import net.bytebuddy.description.modifier.Visibility;
import net.bytebuddy.dynamic.DynamicType;
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
 * 	<li>UtReferrer, UtInheritingReferrer and UtOwnReferrer reference UtEnumDoc's enumerations.
 * 		utcust does not override UtReferrer. Its override of UtInheritingReferrer inherits the
 * 		references from the vanilla class, and its override of UtOwnReferrer declares them in
 * 		UtOwnReferrerOverride. Their fields have the enum types the generated classes would import.</li>
 * </ul>
 * The bean classes are made at run time, and a context class loader serves them and this test's
 * enums by the names of the generated classes, as classes in the <code>modules</code> and
 * <code>customers</code> packages of the test classes would be found as metadata by other tests.
 */
class EnumerationImplementingTypeTest {
	private static final String MODULE = "utmod";
	private static final String OVERRIDING_CUSTOMER = "utcust";

	/** Stands in for <code>modules.utmod.domain.UtEnumDoc$Kind</code>. */
	private enum VanillaDomainClassKind {
		ONE
	}

	/** Stands in for <code>modules.utmod.domain.UtEnumDocVanilla$Status</code>. */
	private enum VanillaClassStatus {
		OPEN
	}

	/** Stands in for <code>modules.utmod.domain.UtEnumDocVanilla$Kept</code>. */
	private enum VanillaClassKept {
		OPEN
	}

	/** Stands in for <code>customers.utcust.modules.utmod.domain.UtEnumDocOverride$Added</code>. */
	private enum OverrideClassAdded {
		OPEN
	}

	/** Stands in for <code>customers.utcust.modules.utmod.domain.UtEnumDocOverride$Kept</code>. */
	private enum OverrideClassKept {
		OPEN
	}

	/** Stands in for <code>modules.utmod.domain.UtExtDocVanilla$Status</code>. */
	private enum ExtendedVanillaClassStatus {
		OPEN
	}

	/** Stands in for <code>customers.utcust.modules.utmod.domain.UtOnlyDoc$Kind</code>. */
	private enum CustomerDocumentKind {
		ONE
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

		put(make("modules.utmod.domain.UtReferrer", Object.class, TypeManifestation.PLAIN,
					Map.of("keptReference", VanillaClassKept.class, "statusReference", VanillaClassStatus.class)));
		Class<?> inheritingReferrer = make("modules.utmod.domain.UtInheritingReferrer", Object.class, TypeManifestation.PLAIN,
											Map.of("keptReference", VanillaClassKept.class));
		put(inheritingReferrer);
		put(make("customers.utcust.modules.utmod.domain.UtInheritingReferrerOverride", inheritingReferrer));
		Class<?> ownReferrer = make("modules.utmod.domain.UtOwnReferrer", Object.class);
		put(ownReferrer);
		put(make("customers.utcust.modules.utmod.domain.UtOwnReferrerOverride", ownReferrer, TypeManifestation.PLAIN,
					Map.of("keptReference", OverrideClassKept.class, "statusReference", VanillaClassStatus.class)));

		CLASSES.put("modules.utmod.domain.UtEnumDoc$Kind", VanillaDomainClassKind.class);
		CLASSES.put("modules.utmod.domain.UtEnumDocVanilla$Status", VanillaClassStatus.class);
		CLASSES.put("modules.utmod.domain.UtEnumDocVanilla$Kept", VanillaClassKept.class);
		CLASSES.put("customers.utcust.modules.utmod.domain.UtEnumDocOverride$Added", OverrideClassAdded.class);
		CLASSES.put("customers.utcust.modules.utmod.domain.UtEnumDocOverride$Kept", OverrideClassKept.class);
		CLASSES.put("modules.utmod.domain.UtExtDocVanilla$Status", ExtendedVanillaClassStatus.class);
		CLASSES.put("customers.utcust.modules.utmod.domain.UtOnlyDoc$Kind", CustomerDocumentKind.class);
	}

	/** The documents utcust overrides or adds. */
	private static final Set<String> OVERRIDDEN = Set.of("UtEnumDoc", "UtExtDoc", "UtOnlyDoc", "UtInheritingReferrer", "UtOwnReferrer");

	/**
	 * Makes a class of the name that extends the superclass.
	 */
	private static Class<?> make(String name, Class<?> superclass) {
		return make(name, superclass, TypeManifestation.PLAIN, Map.of());
	}

	/**
	 * Makes an abstract class of the name that extends the superclass, as a narrowed document's
	 * domain class, and a generated class with a hand-written extension, are abstract.
	 */
	private static Class<?> makeAbstract(String name, Class<?> superclass) {
		return make(name, superclass, TypeManifestation.ABSTRACT, Map.of());
	}

	/**
	 * Makes a class of the name that extends the superclass and declares private fields of the
	 * names and types, as a generated class declares a field for each property.
	 */
	@SuppressWarnings("resource") // the made and loaded types only hold the class bytes in memory
	private static Class<?> make(String name, Class<?> superclass, TypeManifestation manifestation, Map<String, Class<?>> fields) {
		ClassLoader parent = (superclass.getClassLoader() == null) ?
								EnumerationImplementingTypeTest.class.getClassLoader() :
								superclass.getClassLoader();
		// the field types are this test's private enums, which Byte Buddy would refuse as invisible
		DynamicType.Builder<?> builder = new ByteBuddy().with(TypeValidation.DISABLED)
															.subclass(superclass)
															.name(name)
															.modifiers(Visibility.PUBLIC, manifestation);
		for (Map.Entry<String, Class<?>> field : fields.entrySet()) {
			builder = builder.defineField(field.getKey(), field.getValue(), Visibility.PRIVATE);
		}
		return builder.make()
						.load(parent, ClassLoadingStrategy.Default.WRAPPER)
						.getLoaded();
	}

	private static void put(Class<?> type) {
		CLASSES.put(type.getName(), type);
	}

	private ProvidedRepository previousRepository;
	private ProvidedRepository repository;
	private ClassLoader previousClassLoader;
	private final Map<String, DocumentImpl> documents = new HashMap<>();

	@BeforeEach
	void setUp() throws Exception {
		ModuleMemberTestSupport.clearPersistenceThreadLocal();

		previousRepository = ProvidedRepositoryFactory.get();
		repository = mock(ProvidedRepository.class);
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
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		assertAll(() -> assertSame(VanillaDomainClassKind.class, withoutCustomer),
					() -> assertSame(VanillaDomainClassKind.class, kind.getImplementingType()));
	}

	@Test
	void aNarrowedAwayEnumLoadsFromTheVanillaClass() {
		Enumeration status = enumeration("UtEnumDoc", "status");
		Class<?> withoutCustomer = status.getImplementingType();
		ModuleMemberTestSupport.bindCurrentCustomer("other");

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
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		assertSame(ExtendedVanillaClassStatus.class, status.getImplementingType());
	}

	@Test
	void anEnumAnOverrideAddsLoadsFromTheCustomersOverrideClass() {
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		assertSame(OverrideClassAdded.class, enumeration("UtEnumDoc", "added").getImplementingType());
	}

	/**
	 * One enumeration resolves for each customer, so what it loads for one customer is not what it
	 * gives another.
	 */
	@Test
	void anEnumTheOverrideAndTheVanillaClassDeclareLoadsForEachCustomer() {
		Enumeration kept = enumeration("UtEnumDoc", "kept");
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);
		Class<?> overriding = kept.getImplementingType();
		ModuleMemberTestSupport.bindCurrentCustomer("other");
		Class<?> other = kept.getImplementingType();
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		assertAll(() -> assertSame(OverrideClassKept.class, overriding),
					() -> assertSame(VanillaClassKept.class, other),
					() -> assertSame(OverrideClassKept.class, kept.getImplementingType()));
	}

	/** A new customer document's class is in the customer's package. */
	@Test
	void anEnumOfANewCustomerDocumentLoadsFromItsClass() {
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		assertSame(CustomerDocumentKind.class, enumeration("UtOnlyDoc", "kind").getImplementingType());
	}

	/**
	 * Makes an enumeration of the referring document that references UtEnumDoc's enumeration.
	 */
	private Enumeration reference(String documentName, String name, String attributeRef) {
		enumeration("UtEnumDoc", attributeRef);
		Enumeration result = enumeration(documentName, name);
		result.setModuleRef(MODULE);
		result.setDocumentRef("UtEnumDoc");
		result.setAttributeRef(attributeRef);
		return result;
	}

	/**
	 * The type of the field that declares a reference in the bean class of its document for the
	 * customer or the classes it extends, as a generated class imports it.
	 */
	private static Class<?> fieldType(String beanClassName, String fieldName) throws NoSuchFieldException {
		Class<?> type = CLASSES.get(beanClassName);
		while (type != null) {
			for (java.lang.reflect.Field field : type.getDeclaredFields()) {
				if (field.getName().equals(fieldName)) {
					return field.getType();
				}
			}
			type = type.getSuperclass();
		}
		throw new NoSuchFieldException(beanClassName + '.' + fieldName);
	}

	/**
	 * A vanilla class's reference to another document's enumeration loads it from that document's
	 * vanilla classes, as the vanilla class imports it, even for a customer that overrides that
	 * document and declares the enumeration again.
	 */
	@Test
	void anEnumReferencedFromAVanillaClassLoadsFromTheReferencedDocumentsVanillaClasses() throws Exception {
		Enumeration reference = reference("UtReferrer", "keptReference", "kept");
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);
		Class<?> overriding = reference.getImplementingType();
		ModuleMemberTestSupport.bindCurrentCustomer("other");
		Class<?> other = reference.getImplementingType();

		assertAll(() -> assertSame(VanillaClassKept.class, overriding),
					() -> assertSame(VanillaClassKept.class, other),
					() -> assertSame(fieldType("modules.utmod.domain.UtReferrer", "keptReference"), overriding));
	}

	/** A customer's override that inherits a reference from the vanilla class gets the vanilla classes' enum. */
	@Test
	void anEnumReferencedFromAPropertyAnOverrideInheritsLoadsFromTheReferencedDocumentsVanillaClasses() throws Exception {
		Enumeration reference = reference("UtInheritingReferrer", "keptReference", "kept");
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		Class<?> result = reference.getImplementingType();

		assertAll(() -> assertSame(VanillaClassKept.class, result),
					() -> assertSame(fieldType("customers.utcust.modules.utmod.domain.UtInheritingReferrerOverride", "keptReference"), result));
	}

	/** A customer's override that declares a reference itself gets the customer's classes' enum. */
	@Test
	void anEnumReferencedFromAnOverridesOwnPropertyLoadsFromTheReferencedDocumentsCustomerClasses() throws Exception {
		Enumeration reference = reference("UtOwnReferrer", "keptReference", "kept");
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		Class<?> result = reference.getImplementingType();

		assertAll(() -> assertSame(OverrideClassKept.class, result),
					() -> assertSame(fieldType("customers.utcust.modules.utmod.domain.UtOwnReferrerOverride", "keptReference"), result));
	}

	/**
	 * A customer class's reference to an enumeration that the customer's override of the referenced
	 * document leaves out loads it from the vanilla classes, as only they declare it.
	 */
	@Test
	void aNarrowedAwayEnumReferencedFromACustomerClassLoadsFromTheVanillaClass() throws Exception {
		Enumeration reference = reference("UtOwnReferrer", "statusReference", "status");
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		Class<?> result = reference.getImplementingType();

		assertAll(() -> assertSame(VanillaClassStatus.class, result),
					() -> assertSame(fieldType("customers.utcust.modules.utmod.domain.UtOwnReferrerOverride", "statusReference"), result));
	}

	/** A customer given explicitly resolves the enum whoever the current user is. */
	@Test
	void anEnumLoadsForTheCustomerGiven() {
		Enumeration kept = enumeration("UtEnumDoc", "kept");
		Customer overriding = mock(Customer.class);
		when(overriding.getName()).thenReturn(OVERRIDING_CUSTOMER);
		Class<?> withoutUser = kept.getImplementingType(overriding);
		ModuleMemberTestSupport.bindCurrentCustomer("other");

		assertAll(() -> assertSame(OverrideClassKept.class, withoutUser),
					() -> assertSame(OverrideClassKept.class, kept.getImplementingType(overriding)),
					() -> assertSame(VanillaClassKept.class, kept.getImplementingType(null)),
					() -> assertSame(VanillaClassKept.class, kept.getImplementingType()));
	}

	/** The enum class is cached for the customer's name, so neither the customer nor its bean class is resolved again. */
	@Test
	void aCachedEnumIsFoundWithoutResolvingTheCustomer() {
		Enumeration kept = enumeration("UtEnumDoc", "kept");
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);
		Class<?> first = kept.getImplementingType();
		User user = AbstractPersistence.get().getUser();
		clearInvocations(user, repository);

		Class<?> second = kept.getImplementingType();

		assertAll(() -> assertSame(OverrideClassKept.class, first),
					() -> assertSame(first, second),
					() -> verify(user, never()).getCustomer(),
					() -> verifyNoInteractions(repository));
	}

	/** A class cached through one context class loader is looked for again through another. */
	@Test
	void anEnumCachedThroughAnotherClassLoaderIsResolvedAgain() {
		Enumeration kind = enumeration("UtEnumDoc", "kind");
		Class<?> cached = kind.getImplementingType();
		Thread.currentThread().setContextClassLoader(new ClassLoader(previousClassLoader) {
			@Override
			protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
				if (name.startsWith("modules.") || name.startsWith("customers.")) {
					throw new ClassNotFoundException(name);
				}
				return super.loadClass(name, resolve);
			}
		});

		MetaDataException e = assertThrows(MetaDataException.class, kind::getImplementingType);

		assertAll(() -> assertSame(VanillaDomainClassKind.class, cached),
					() -> assertTrue(Enumeration.isEnumClassLoadingFailure(e), e::toString));
	}

	/**
	 * Without a repository, as in a unit test that installs none, the enum loads from the vanilla
	 * domain class, and one that it does not declare is an enum class loading failure.
	 */
	@Test
	void withoutARepositoryAnEnumLoadsFromTheVanillaDomainClass() {
		Enumeration kind = enumeration("UtEnumDoc", "kind");
		Enumeration status = enumeration("UtEnumDoc", "status");
		ProvidedRepositoryFactory.set(null);
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);

		Class<?> result = kind.getImplementingType();
		MetaDataException e = assertThrows(MetaDataException.class, status::getImplementingType);

		assertAll(() -> assertSame(VanillaDomainClassKind.class, result),
					() -> assertEquals("A problem was encountered loading enum modules.utmod.domain.UtEnumDoc$Status", e.getMessage()),
					() -> assertTrue(Enumeration.isEnumClassLoadingFailure(e), e::toString));
	}

	@Test
	void anUndeclaredEnumThrowsAnEnumClassLoadingFailure() {
		ModuleMemberTestSupport.bindCurrentCustomer(OVERRIDING_CUSTOMER);
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
