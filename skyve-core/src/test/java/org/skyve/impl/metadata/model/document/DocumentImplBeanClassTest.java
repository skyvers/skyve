package org.skyve.impl.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.repository.ProvidedRepository;

/**
 * Tests how {@link DocumentImpl#getBeanClass(Customer)} chooses between the generated domain classes,
 * the generated <code>&lt;Document&gt;Vanilla</code> and <code>&lt;Document&gt;Override</code> classes and
 * hand-written <code>&lt;Document&gt;Extension</code> classes. A context class loader serves the nested
 * fixture classes under the names Skyve looks up, so no fixture packages sit on the test class path
 * (a <code>customers</code> package there would look like a customer to the repository tests).
 */
@SuppressWarnings("static-method")
class DocumentImplBeanClassTest {
	private static final String MODULE = "beanclassfixture";

	/** A concrete generated domain class. */
	public static class Item {
		// fixture
	}

	/** A generated domain class made abstract by a customer override that leaves out an attribute. */
	public abstract static class Order {
		// fixture
	}

	/** The concrete vanilla class generated for the narrowed Order. */
	public static class OrderVanilla extends Order {
		// fixture
	}

	/** The class generated for customer acme's override of Order. */
	public static class OrderOverride extends Order {
		// fixture
	}

	/** A generated domain class, abstract because it has a hand-written extension. */
	public abstract static class Invoice {
		// fixture
	}

	/** A hand-written extension. */
	public static class InvoiceExtension extends Invoice {
		// fixture
	}

	/** The class generated for customer acme's override of Invoice, which extends the hand-written extension. */
	public static class InvoiceOverride extends InvoiceExtension {
		// fixture
	}

	/** A generated domain class with a hand-written extension, narrowed by a customer override. */
	public abstract static class Receipt {
		// fixture
	}

	/** A hand-written extension of a narrowed document. */
	public static class ReceiptExtension extends Receipt {
		// fixture
	}

	/** The concrete vanilla class generated for the narrowed Receipt, which carries the narrowed-away properties. */
	public static class ReceiptVanilla extends ReceiptExtension {
		// fixture
	}

	/** The fixture classes by the names Skyve looks them up by. */
	private static final Map<String, Class<?>> CLASSES = Map.ofEntries(
			Map.entry("modules.beanclassfixture.domain.Item", Item.class),
			Map.entry("modules.beanclassfixture.domain.Order", Order.class),
			Map.entry("modules.beanclassfixture.domain.OrderVanilla", OrderVanilla.class),
			Map.entry("customers.acme.modules.beanclassfixture.domain.OrderOverride", OrderOverride.class),
			Map.entry("modules.beanclassfixture.domain.Invoice", Invoice.class),
			Map.entry("modules.beanclassfixture.Invoice.InvoiceExtension", InvoiceExtension.class),
			Map.entry("customers.acme.modules.beanclassfixture.domain.InvoiceOverride", InvoiceOverride.class),
			Map.entry("modules.beanclassfixture.domain.Receipt", Receipt.class),
			Map.entry("modules.beanclassfixture.Receipt.ReceiptExtension", ReceiptExtension.class),
			Map.entry("modules.beanclassfixture.domain.ReceiptVanilla", ReceiptVanilla.class));

	/** The documents each customer overrides - acme has generated override classes, beta none. */
	private static final Map<String, Set<String>> OVERRIDES = Map.of("acme", Set.of("Item", "Order", "Invoice"),
																		"beta", Set.of("Invoice"));

	/** The documents with a hand-written vanilla extension. */
	private static final Set<String> EXTENDED = Set.of("Invoice", "Receipt");

	private ProvidedRepository previousRepository;
	private ClassLoader previousClassLoader;

	@BeforeEach
	void installRepository() {
		previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.vtable(anyString(), anyString())).thenAnswer(invocation -> {
			String customerName = invocation.getArgument(0);
			String key = invocation.getArgument(1);
			String[] parts = key.split("/");
			String documentName = parts[2];
			if (parts.length == 4) { // modules/<m>/<Doc>/<Doc>Extension
				return EXTENDED.contains(documentName) ? key : null;
			}
			return OVERRIDES.getOrDefault(customerName, Set.of()).contains(documentName) ? "customers/" + customerName + '/' + key : key;
		});
		ProvidedRepositoryFactory.set(repository);

		previousClassLoader = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(new ClassLoader(previousClassLoader) {
			@Override
			public Class<?> loadClass(String name) throws ClassNotFoundException {
				Class<?> result = CLASSES.get(name);
				if (result != null) {
					return result;
				}
				if (name.startsWith("modules.") || name.startsWith("customers.")) {
					throw new ClassNotFoundException(name);
				}
				return super.loadClass(name);
			}
		});
	}

	@AfterEach
	void restoreRepository() {
		Thread.currentThread().setContextClassLoader(previousClassLoader);
		ProvidedRepositoryFactory.set(previousRepository);
	}

	private static Class<?> beanClass(String customerName, String documentName) throws Exception {
		DocumentImpl document = new DocumentImpl();
		document.setName(documentName);
		document.setOwningModuleName(MODULE);
		Customer customer = mock(Customer.class);
		when(customer.getName()).thenReturn(customerName);
		return document.getBeanClass(customer);
	}

	@Test
	void aVanillaDocumentUsesItsDomainClass() throws Exception {
		assertEquals(Item.class, beanClass("beta", "Item"));
	}

	@Test
	void aNarrowedVanillaDocumentUsesItsVanillaClass() throws Exception {
		assertEquals(OrderVanilla.class, beanClass("beta", "Order"));
	}

	@Test
	void aVanillaDocumentWithAnExtensionUsesTheExtension() throws Exception {
		assertEquals(InvoiceExtension.class, beanClass("other", "Invoice"));
	}

	@Test
	void anOverrideUsesItsOverrideClass() throws Exception {
		assertEquals(OrderOverride.class, beanClass("acme", "Order"));
	}

	@Test
	void anOverrideWithoutAnOverrideClassUsesTheVanillaDomainClass() throws Exception {
		assertEquals(Item.class, beanClass("acme", "Item"));
	}

	@Test
	void anOverrideOfADocumentWithAnExtensionUsesItsOverrideClass() throws Exception {
		// the generated override class extends the hand-written vanilla extension
		assertEquals(InvoiceOverride.class, beanClass("acme", "Invoice"));
	}

	@Test
	void anOverrideWithoutAnOverrideClassOfADocumentWithAnExtensionUsesTheExtension() throws Exception {
		assertEquals(InvoiceExtension.class, beanClass("beta", "Invoice"));
	}

	@Test
	@Disabled("Item C2(b): a narrowed document with a hand-written extension - awaiting a design decision")
	void aNarrowedVanillaDocumentWithAnExtensionUsesItsVanillaClass() throws Exception {
		// ReceiptVanilla extends ReceiptExtension and carries the properties the narrowing took off
		// Receipt, but the vanilla extension is found first
		assertEquals(ReceiptVanilla.class, beanClass("beta", "Receipt"));
	}
}
