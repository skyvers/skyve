package util;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import java.util.List;

import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.util.TestUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;
import org.skyve.util.Util;

public abstract class AbstractDomainTest<T extends PersistentBean> extends AbstractH2Test {

	protected abstract T getBean() throws Exception;

	@Test
	@SuppressWarnings("boxing")
	public void testDelete() throws Exception {
		// create the test data
		T bean = getBean();

		assertThat(bean.isPersisted(), is(false));

		T result = CORE.getPersistence().save(bean);

		// validate the test data
		assertThat(result, is(notNullValue()));
		assertThat(result.isPersisted(), is(true));
		assertThat(result.getBizId(), is(notNullValue()));

		// perform the method under test
		CORE.getPersistence().delete(result);

		// verify the results
		CORE.getPersistence().evictAllCached();
		T deleted = CORE.getPersistence().retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId(), false);

		assertThat(deleted, is(nullValue()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindAll() throws Exception {
		// create the test data
		T b1 = getBean();
		T b2 = getBean();

		int beanCount = CORE.getPersistence().newDocumentQuery(b1.getBizModule(), b1.getBizDocument()).beanResults().size();

		CORE.getPersistence().save(b1);
		CORE.getPersistence().save(b2);

		// perform the method under test
		List<T> results = CORE.getPersistence().newDocumentQuery(b1.getBizModule(), b1.getBizDocument()).beanResults();

		// verify the results
		assertThat(results.size(), is(beanCount + 2));
	}

	@Test
	public void testFindById() throws Exception {
		// create the test data
		T bean = getBean();

		CORE.getPersistence().save(bean);

		// perform the method under test
		T result = CORE.getPersistence().retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId(), false);

		// verify the results
		assertThat(result, is(notNullValue()));
		assertThat(result.getBizId(), is(bean.getBizId()));
		assertThat(result, is(bean));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testSave() throws Exception {
		// create the test data
		T bean = getBean();

		// validate the test data
		assertThat(bean.isPersisted(), is(false));

		// perform the method under test
		T result = CORE.getPersistence().save(bean);

		// verify the results
		assertThat(result, is(notNullValue()));
		assertThat(result.isPersisted(), is(true));
		assertThat(result.getBizId(), is(notNullValue()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testUpdate() throws Exception {
		// create the test data
		T bean = getBean();

		// validate the test data
		assertThat(bean.isPersisted(), is(false));

		// perform the method under test
		T result = CORE.getPersistence().save(bean);

		// verify the results
		assertThat(result, is(notNullValue()));
		assertThat(result.isPersisted(), is(true));
		assertThat(result.getBizId(), is(notNullValue()));

		// perform an update
		Attribute attributeToUpdate = getRandomAttribute(result);

		if (attributeToUpdate != null) {
			Object originalValue = Binder.get(result, attributeToUpdate.getName());
			TestUtil.updateAttribute(result, attributeToUpdate);
			T uResult = CORE.getPersistence().save(result);

			// verify the results
			assertThat(Binder.get(uResult, attributeToUpdate.getName()), is(not(originalValue)));
		} else {
			Util.LOGGER.fine(String.format("Skipping update test for %s, no scalar attribute found", bean.getBizDocument()));
		}
	}

	private Attribute getRandomAttribute(T bean) {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());

		for (Attribute attribute : document.getAllAttributes()) {
			AttributeType type = attribute.getAttributeType();

			if (AttributeType.collection.equals(type) || AttributeType.association.equals(type)
					|| AttributeType.inverseOne.equals(type) || AttributeType.inverseMany.equals(type)) {
				continue;
			}

			return attribute;
		}

		return null;
	}
}
