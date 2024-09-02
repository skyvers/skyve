package util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assume.assumeTrue;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.model.document.field.Enumeration;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.UsageType;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.util.test.TestUtil;

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
		T deleted = CORE.getPersistence().retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId());

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

		try {
			CORE.getPersistence().save(b2);
		} catch (@SuppressWarnings("unused") UniqueConstraintViolationException uce) {
			// failed to create a unique second bean, try create b2 again
			// if this happens consistently, you may need to use a factory to create unique instances of this document
			b2 = getBean();
			CORE.getPersistence().save(b2);
		}

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
		T result = CORE.getPersistence().retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId());

		// verify the results
		assertThat(result, is(notNullValue()));
		assertThat(result.getBizId(), is(bean.getBizId()));
		assertThat(result, is(bean));
	}

	@Test
	public void testGetConstantDomainValues() throws Exception {
		assumeTrue(getBizlet() != null);

		// create the test data
		ArrayList<? extends Attribute> allAttributes = getAllAttributes(getBean());

		// perform the method under test
		for (Attribute attribute : allAttributes) {
			AttributeType type = attribute.getAttributeType();

			// skip updating content, an association or collection, try find a scalar attribute to update
			if (AttributeType.content.equals(type) ||
					AttributeType.image.equals(type) ||
					AttributeType.collection.equals(type) ||
					AttributeType.association.equals(type) ||
					AttributeType.inverseOne.equals(type) ||
					AttributeType.inverseMany.equals(type)) {
				continue;
			}

			try {
				getBizlet().getConstantDomainValues(attribute.getName());
			} catch (@SuppressWarnings("unused") ValidationException e) {
				// pass - bizlet validated incorrect input
			}
		}
	}

	@Test
	public void testGetDynamicDomainValues() throws Exception {
		assumeTrue(getBizlet() != null);

		// create the test data
		ArrayList<? extends Attribute> allAttributes = getAllAttributes(getBean());

		// perform the method under test
		for (Attribute attribute : allAttributes) {
			AttributeType type = attribute.getAttributeType();

			// skip updating content, an association or collection, try find a scalar attribute to update
			if (AttributeType.content.equals(type) ||
					AttributeType.image.equals(type) ||
					AttributeType.collection.equals(type) ||
					AttributeType.association.equals(type) ||
					AttributeType.inverseOne.equals(type) ||
					AttributeType.inverseMany.equals(type)) {
				continue;
			}

			try {
				getBizlet().getDynamicDomainValues(attribute.getName(), getBean());
			} catch (@SuppressWarnings("unused") ValidationException e) {
				// pass - bizlet validated incorrect input
			}
		}
	}

	@Test
	public void testGetVariantDomainValues() throws Exception {
		assumeTrue(getBizlet() != null);
		// create the test data
		T bean = getBean();
		ArrayList<? extends Attribute> allAttributes = getAllAttributes(bean);

		// perform the method under test
		for (Attribute attribute : allAttributes) {
			AttributeType type = attribute.getAttributeType();

			// skip updating content, an association or collection, try find a scalar attribute to update
			if (AttributeType.content.equals(type) ||
					AttributeType.image.equals(type) ||
					AttributeType.collection.equals(type) ||
					AttributeType.association.equals(type) ||
					AttributeType.inverseOne.equals(type) ||
					AttributeType.inverseMany.equals(type)) {
				continue;
			}

			try {
				getBizlet().getVariantDomainValues(attribute.getName());
			} catch (@SuppressWarnings("unused") ValidationException e) {
				// pass - bizlet validated incorrect input
			}
		}
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
	@Timeout(30)
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

			Customer customer = CORE.getUser().getCustomer();
			Module module = customer.getModule(getBean().getBizModule());
			Document document = module.getDocument(customer, getBean().getBizDocument());

			TestUtil.updateAttribute(module, document, result, attributeToUpdate);
			
			if (Objects.equals(Binder.get(result, attributeToUpdate.getName()), originalValue)) {
				// skip this test if we couldn't generate a new value to save
				Util.LOGGER.warning(String.format("Skipping testUpdate() for attribute %s, original and updated values were the same", attributeToUpdate.getName()));
				return;
			}
			
			T uResult = CORE.getPersistence().save(result);

			// verify the results
			assertThat("Error updating " + attributeToUpdate.getName(), Binder.get(uResult, attributeToUpdate.getName()),
						is(not(originalValue)));
		} else {
			Util.LOGGER.fine(String.format("Skipping update test for %s, no scalar attribute found", bean.getBizDocument()));
		}
	}

	private ArrayList<? extends Attribute> getAllAttributes(T bean) {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		ArrayList<? extends Attribute> allAttributes = new ArrayList<>(document.getAllAttributes(customer));
		return allAttributes;
	}

	private Bizlet<T> getBizlet() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(getBean().getBizModule());
		Document document = module.getDocument(customer, getBean().getBizDocument());

		return document.getBizlet(customer);
	}

	private Attribute getRandomAttribute(T bean) {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(bean.getBizModule());
		Document document = module.getDocument(customer, bean.getBizDocument());
		Attribute transientOrViewAttribute = null;

		ArrayList<? extends Attribute> allAttributes = new ArrayList<>(document.getAllAttributes(customer));

		// remove any excluded attributes
		List<String> excludedAttributes = TestUtil.retrieveExcludedUpdateAttributes(module, document);
		if (!excludedAttributes.isEmpty()) {
			allAttributes.removeIf(a -> excludedAttributes.contains(a.getName()));
		}

		// randomise the attributes in the collection
		Collections.shuffle(allAttributes);

		for (Attribute attribute : allAttributes) {
			AttributeType type = attribute.getAttributeType();

			// skip updating content, an association or collection, try find a scalar attribute to update
			if (AttributeType.content.equals(type) ||
					AttributeType.image.equals(type) ||
					AttributeType.collection.equals(type) ||
					AttributeType.association.equals(type) ||
					AttributeType.inverseOne.equals(type) ||
					AttributeType.inverseMany.equals(type)) {
				continue;
			}

			// if this enum only has 1 value, use a different attribute
			if (AttributeType.enumeration.equals(type)) {
				Enumeration e = (Enumeration) attribute;
				if (e.getValues().size() == 1) {
					continue;
				}
			}

			// if this is a domain attribute (constant, dynamic or variant), use a different attribute
			if (attribute.getDomainType() != null) {
				continue;
			}

			// try not to use a view attribute if we can
			if (attribute.getUsage() == UsageType.view) {
				transientOrViewAttribute = attribute;
				continue;
			}

			// try use a persistent attribute if we can
			if (!attribute.isPersistent()) {
				transientOrViewAttribute = attribute;
				continue;
			}

			return attribute;
		}

		return transientOrViewAttribute != null ? transientOrViewAttribute : null;
	}
}
