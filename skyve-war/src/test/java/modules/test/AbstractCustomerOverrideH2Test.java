package modules.test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.model.document.DocumentImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.ModuleDocument;
import org.skyve.persistence.Persistence;

import util.AbstractH2Test;

/**
 * Support for H2 tests of customer overrides, which switch the customer a test runs as. The vanilla
 * mappings and every customer's <code>customers/&lt;customer&gt;/modules/orm.hbm.xml</code> share one
 * SessionFactory, so a subclass starting at all shows that they load together.
 */
abstract class AbstractCustomerOverrideH2Test extends AbstractH2Test {
	/** The customer whose overrides of the test module are the fixtures. */
	protected static final String TEST_CUSTOMER = "test";

	/**
	 * Runs the rest of the test as a super user of the customer, in the current transaction.
	 *
	 * @param customerName	The customer.
	 * @return	The customer.
	 */
	protected static Customer runAs(String customerName) {
		Customer result = customer(customerName);
		SuperUser user = new SuperUser();
		user.setCustomerName(customerName);
		user.setName(USER);
		user.setId(USER);
		AbstractPersistence.get().setUser(user);
		return result;
	}

	/**
	 * The customer's metadata.
	 *
	 * @param customerName	The customer.
	 * @return	The customer.
	 */
	protected static Customer customer(String customerName) {
		Customer result = ProvidedRepositoryFactory.getNullSafe().getCustomer(customerName);
		assertNotNull(result, customerName);
		return result;
	}

	/**
	 * Creates a bean of the document for the current user's customer.
	 *
	 * @param document	The document.
	 * @return	The new bean.
	 * @throws Exception	if the bean cannot be created.
	 */
	protected static <T extends Bean> T newInstance(ModuleDocument document) throws Exception {
		return document.toDocument().newInstance(CORE.getUser());
	}

	/**
	 * The class of the customer's beans of the document.
	 *
	 * @param document	The document.
	 * @param customer	The customer.
	 * @return	The bean class.
	 * @throws Exception	if the class cannot be loaded.
	 */
	protected static Class<?> beanClass(ModuleDocument document, Customer customer) throws Exception {
		return ((DocumentImpl) document.toDocument(customer)).getBeanClass(customer);
	}

	/**
	 * Saves the bean, clears the first level cache, and retrieves the bean again from the data store.
	 *
	 * @param document	The bean's document.
	 * @param bean	The bean to save.
	 * @return	The retrieved bean.
	 */
	protected static <T extends PersistentBean> T saveAndRetrieve(ModuleDocument document, T bean) {
		Persistence p = CORE.getPersistence();
		T saved = p.save(bean);
		p.evictAllCached();
		T result = p.retrieve(document, saved.getBizId());
		assertNotNull(result, "retrieve " + document.documentName());
		return result;
	}
}
