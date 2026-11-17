package org.skyve.impl.metadata.model.document.field;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.user.User;

/**
 * Tests that {@link Enumeration#getImplementingType()} loads a generated enum from the class that
 * declares it: the vanilla domain class, the customer's <code>&lt;Document&gt;Override</code> class
 * or the <code>&lt;Document&gt;Vanilla</code> class, as the domain generator declares them. The
 * context class loader loads this test's enums by the names of the generated enums they stand in
 * for, as classes in the <code>modules</code> and <code>customers</code> packages of the test
 * classes would be found as metadata by other tests.
 */
@SuppressWarnings("static-method")
class EnumerationImplementingTypeTest {
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

	/**
	 * Loads the stand-in enums by the names of the generated enums, and no other class of the
	 * <code>modules</code> or <code>customers</code> packages. The document UtEnumDoc in the module
	 * utmod is abstract, as the utcust customer's override leaves out its status and kept
	 * attributes, so UtEnumDocVanilla declares them for other customers, and the override adds the
	 * attribute added and declares kept itself.
	 */
	private static final class GeneratedClassLoader extends ClassLoader {
		private static final Map<String, Class<?>> CLASSES = Map.of("modules.utmod.domain.UtEnumDoc$Kind", VanillaDomainClassKind.class,
																		"modules.utmod.domain.UtEnumDocVanilla$Status", VanillaClassStatus.class,
																		"modules.utmod.domain.UtEnumDocVanilla$Kept", VanillaClassKept.class,
																		"customers.utcust.modules.utmod.domain.UtEnumDocOverride$Added", OverrideClassAdded.class,
																		"customers.utcust.modules.utmod.domain.UtEnumDocOverride$Kept", OverrideClassKept.class);

		private GeneratedClassLoader(ClassLoader parent) {
			super(parent);
		}

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
	}

	private ClassLoader contextClassLoader;

	@BeforeEach
	void setUp() {
		threadLocalPersistence().remove();
		Thread thread = Thread.currentThread();
		contextClassLoader = thread.getContextClassLoader();
		thread.setContextClassLoader(new GeneratedClassLoader(contextClassLoader));
	}

	@AfterEach
	void tearDown() {
		threadLocalPersistence().remove();
		Thread.currentThread().setContextClassLoader(contextClassLoader);
	}

	@SuppressWarnings("unchecked")
	private static ThreadLocal<AbstractPersistence> threadLocalPersistence() {
		try {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			return (ThreadLocal<AbstractPersistence>) field.get(null);
		}
		catch (ReflectiveOperationException e) {
			throw new AssertionError(e);
		}
	}

	/**
	 * Runs the rest of the test as a user of the customer.
	 */
	private static void runAs(String customerName) {
		User user = mock(User.class);
		when(user.getCustomerName()).thenReturn(customerName);
		AbstractPersistence persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
		persistence.setUser(user);
		persistence.setForThread();
	}

	/**
	 * An enumeration attribute of the document UtEnumDoc in the module utmod.
	 */
	private static Enumeration enumeration(String name) {
		Document document = mock(Document.class);
		when(document.getOwningModuleName()).thenReturn("utmod");
		when(document.getName()).thenReturn("UtEnumDoc");
		Enumeration result = new Enumeration();
		result.setName(name);
		result.setOwningDocument(document);
		return result;
	}

	@Test
	void anEnumOfTheVanillaDomainClassLoadsFromIt() {
		Enumeration kind = enumeration("kind");
		Class<?> withoutCustomer = kind.getImplementingType();
		runAs(OVERRIDING_CUSTOMER);

		assertAll(() -> assertSame(VanillaDomainClassKind.class, withoutCustomer),
					() -> assertSame(VanillaDomainClassKind.class, kind.getImplementingType()));
	}

	@Test
	void aNarrowedAwayEnumLoadsFromTheVanillaClass() {
		Enumeration status = enumeration("status");
		Class<?> withoutCustomer = status.getImplementingType();
		runAs(OVERRIDING_CUSTOMER);

		assertAll(() -> assertSame(VanillaClassStatus.class, withoutCustomer),
					() -> assertSame(VanillaClassStatus.class, status.getImplementingType()));
	}

	@Test
	void anEnumAnOverrideAddsLoadsFromTheCustomersOverrideClass() {
		runAs(OVERRIDING_CUSTOMER);

		assertSame(OverrideClassAdded.class, enumeration("added").getImplementingType());
	}

	/**
	 * One enumeration resolves for each customer, so what it loads for one customer is not what it
	 * gives another.
	 */
	@Test
	void anEnumTheOverrideAndTheVanillaClassDeclareLoadsForEachCustomer() {
		Enumeration kept = enumeration("kept");
		runAs(OVERRIDING_CUSTOMER);
		Class<?> overriding = kept.getImplementingType();
		runAs("other");
		Class<?> other = kept.getImplementingType();
		runAs(OVERRIDING_CUSTOMER);

		assertAll(() -> assertSame(OverrideClassKept.class, overriding),
					() -> assertSame(VanillaClassKept.class, other),
					() -> assertSame(OverrideClassKept.class, kept.getImplementingType()));
	}

	@Test
	void anUndeclaredEnumThrowsAnEnumClassLoadingFailure() {
		runAs(OVERRIDING_CUSTOMER);
		Enumeration missing = enumeration("missing");

		MetaDataException e = assertThrows(MetaDataException.class, missing::getImplementingType);

		assertTrue(Enumeration.isEnumClassLoadingFailure(e), e::toString);
	}
}
