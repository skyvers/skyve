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

import customers.acme.modules.beanclassfixture.domain.InvoiceOverride;
import customers.acme.modules.beanclassfixture.domain.OrderOverride;
import modules.beanclassfixture.Invoice.InvoiceExtension;
import modules.beanclassfixture.domain.Item;
import modules.beanclassfixture.domain.OrderVanilla;
import modules.beanclassfixture.domain.ReceiptVanilla;

/**
 * Tests how {@link DocumentImpl#getBeanClass(Customer)} chooses between the generated domain classes,
 * the generated <code>&lt;Document&gt;Vanilla</code> and <code>&lt;Document&gt;Override</code> classes and
 * hand-written <code>&lt;Document&gt;Extension</code> classes, using the fixture classes in the
 * <code>modules.beanclassfixture</code> and <code>customers.acme.modules.beanclassfixture</code> packages.
 */
@SuppressWarnings("static-method")
class DocumentImplBeanClassTest {
	private static final String MODULE = "beanclassfixture";

	/** The documents each customer overrides - acme has generated override classes, beta none. */
	private static final Map<String, Set<String>> OVERRIDES = Map.of("acme", Set.of("Item", "Order", "Invoice"),
																		"beta", Set.of("Invoice"));

	/** The documents with a hand-written vanilla extension. */
	private static final Set<String> EXTENDED = Set.of("Invoice", "Receipt");

	private ProvidedRepository previousRepository;

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
	}

	@AfterEach
	void restoreRepository() {
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
