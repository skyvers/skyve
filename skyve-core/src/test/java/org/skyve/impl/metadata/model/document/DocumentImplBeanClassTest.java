package org.skyve.impl.metadata.model.document;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.util.UtilImpl;
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

	/** The abstract vanilla class generated for the narrowed Receipt, which carries the narrowed-away properties. */
	public abstract static class ReceiptVanilla extends Receipt {
		// fixture
	}

	/** A hand-written extension of a narrowed document, which extends its vanilla class. */
	public static class ReceiptExtension extends ReceiptVanilla {
		// fixture
	}

	/** A generated domain class. */
	public static class Payment {
		// fixture
	}

	/** The abstract class generated for customer acme's override of Payment, as acme has its own extension. */
	public abstract static class PaymentOverride extends Payment {
		// fixture
	}

	/** Customer acme's own hand-written extension of its Payment override. */
	public static class AcmePaymentExtension extends PaymentOverride {
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
			Map.entry("modules.beanclassfixture.domain.ReceiptVanilla", ReceiptVanilla.class),
			Map.entry("modules.beanclassfixture.domain.Payment", Payment.class),
			Map.entry("customers.acme.modules.beanclassfixture.domain.PaymentOverride", PaymentOverride.class),
			Map.entry("customers.acme.modules.beanclassfixture.Payment.PaymentExtension", AcmePaymentExtension.class));

	/** The documents each customer overrides - acme has generated override classes, beta none. */
	private static final Map<String, Set<String>> OVERRIDES = Map.of("acme", Set.of("Item", "Order", "Invoice", "Payment"),
																		"beta", Set.of("Invoice"));

	/** The documents with a hand-written vanilla extension. */
	private static final Set<String> EXTENDED = Set.of("Invoice", "Receipt");

	/** The documents customer acme has its own hand-written extension of. */
	private static final Set<String> ACME_EXTENDED = Set.of("Payment");

	/**
	 * Serves the fixture classes by the names Skyve looks them up by, and records the names in the
	 * <code>modules</code> and <code>customers</code> packages it has no class for.
	 */
	private final class FixtureClassLoader extends ClassLoader {
		private final Map<String, Class<?>> classes;

		private FixtureClassLoader(ClassLoader parent, Map<String, Class<?>> classes) {
			super(parent);
			this.classes = classes;
		}

		@Override
		public Class<?> loadClass(String name) throws ClassNotFoundException {
			Class<?> result = classes.get(name);
			if (result != null) {
				return result;
			}
			if (name.startsWith("modules.") || name.startsWith("customers.")) {
				missingClassNames.add(name);
				throw new ClassNotFoundException(name);
			}
			return super.loadClass(name);
		}
	}

	private ProvidedRepository previousRepository;
	private ClassLoader previousClassLoader;
	private final List<String> missingClassNames = new ArrayList<>();

	@BeforeEach
	void installRepository() {
		previousRepository = ProvidedRepositoryFactory.get();
		ProvidedRepository repository = mock(ProvidedRepository.class);
		when(repository.vtable(any(), anyString())).thenAnswer(invocation -> {
			String customerName = invocation.getArgument(0);
			String key = invocation.getArgument(1);
			String[] parts = key.split("/");
			String documentName = parts[2];
			if (parts.length == 4) { // modules/<m>/<Doc>/<Doc>Extension
				if ("acme".equals(customerName) && ACME_EXTENDED.contains(documentName)) {
					return "customers/acme/" + key;
				}
				return EXTENDED.contains(documentName) ? key : null;
			}
			boolean overridden = (customerName != null) && OVERRIDES.getOrDefault(customerName, Set.of()).contains(documentName);
			return overridden ? "customers/" + customerName + '/' + key : key;
		});
		ProvidedRepositoryFactory.set(repository);

		previousClassLoader = Thread.currentThread().getContextClassLoader();
		Thread.currentThread().setContextClassLoader(new FixtureClassLoader(previousClassLoader, CLASSES));
	}

	@AfterEach
	void restoreRepository() {
		Thread.currentThread().setContextClassLoader(previousClassLoader);
		ProvidedRepositoryFactory.set(previousRepository);
	}

	private static Class<?> beanClass(String customerName, String documentName) throws Exception {
		return document(documentName).getBeanClass(customer(customerName));
	}

	private static DocumentImpl document(String documentName) {
		DocumentImpl result = new DocumentImpl();
		result.setName(documentName);
		result.setOwningModuleName(MODULE);
		return result;
	}

	private static Customer customer(String customerName) {
		Customer result = mock(Customer.class);
		when(result.getName()).thenReturn(customerName);
		return result;
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
	void anOverrideWithTheCustomersOwnExtensionUsesThatExtension() throws Exception {
		assertEquals(AcmePaymentExtension.class, beanClass("acme", "Payment"));
	}

	@Test
	void aDocumentACustomerHasItsOwnExtensionOfUsesTheDomainClassForOtherCustomers() throws Exception {
		assertEquals(Payment.class, beanClass("beta", "Payment"));
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
	void aNarrowedVanillaDocumentWithAnExtensionUsesItsExtension() throws Exception {
		// Receipt <- ReceiptVanilla <- ReceiptExtension: the extension has every vanilla property,
		// through ReceiptVanilla
		assertEquals(ReceiptExtension.class, beanClass("beta", "Receipt"));
	}

	@Test
	void withoutACustomerADocumentUsesItsVanillaBeanClass() throws Exception {
		assertAll(() -> assertEquals(OrderVanilla.class, document("Order").getBeanClass(null)),
					() -> assertEquals(InvoiceExtension.class, document("Invoice").getBeanClass(null)));
	}

	/**
	 * Customer beta overrides Invoice without an override class, so its bean class is the vanilla
	 * extension, found after looking for the customer's override and domain classes.
	 */
	@Test
	void theClassesACustomerDoesNotHaveAreLookedForOnce() throws Exception {
		DocumentImpl invoice = document("Invoice");
		Customer beta = customer("beta");
		Class<?> first = invoice.getBeanClass(beta);
		Class<?> second = invoice.getBeanClass(beta);

		assertAll(() -> assertEquals(InvoiceExtension.class, first),
					() -> assertSame(first, second),
					() -> assertEquals(List.of("customers.beta.modules.beanclassfixture.domain.InvoiceOverride",
												"customers.beta.modules.beanclassfixture.domain.Invoice"),
										missingClassNames));
	}

	@Test
	void eachCustomerGetsItsOwnBeanClass() throws Exception {
		DocumentImpl order = document("Order");
		Class<?> acme = order.getBeanClass(customer("acme"));
		Class<?> beta = order.getBeanClass(customer("beta"));

		assertAll(() -> assertEquals(OrderOverride.class, acme),
					() -> assertEquals(OrderVanilla.class, beta),
					() -> assertEquals(OrderOverride.class, order.getBeanClass(customer("acme"))));
	}

	/** Classes reloaded through another context class loader are not the ones resolved before. */
	@Test
	void anotherContextClassLoaderResolvesTheBeanClassAgain() throws Exception {
		DocumentImpl item = document("Item");
		Customer beta = customer("beta");
		Class<?> before = item.getBeanClass(beta);
		Thread.currentThread().setContextClassLoader(new FixtureClassLoader(previousClassLoader, Map.of("modules.beanclassfixture.domain.Item", Payment.class)));

		assertAll(() -> assertEquals(Item.class, before),
					() -> assertEquals(Payment.class, item.getBeanClass(beta)));
	}

	/** In dev mode the classes can be generated and reloaded, so nothing is cached. */
	@Test
	void devModeResolvesTheBeanClassEachTime() throws Exception {
		DocumentImpl invoice = document("Invoice");
		Customer beta = customer("beta");
		boolean devMode = UtilImpl.DEV_MODE;
		UtilImpl.DEV_MODE = true;
		try {
			invoice.getBeanClass(beta);
			invoice.getBeanClass(beta);
		}
		finally {
			UtilImpl.DEV_MODE = devMode;
		}

		assertEquals(4, missingClassNames.size(), missingClassNames::toString);
	}
}
