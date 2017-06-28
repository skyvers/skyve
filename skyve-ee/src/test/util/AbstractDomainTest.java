package util;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import java.security.SecureRandom;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.bind.BindUtil;
import org.skyve.impl.metadata.model.document.field.LengthField;
import org.skyve.impl.util.TimeUtil;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Binder;
import org.skyve.util.Util;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;

public abstract class AbstractDomainTest<T extends PersistentBean> extends AbstractH2Test {

	protected abstract T getBean() throws Exception;

	private static final SecureRandom random = new SecureRandom();

	@Before
	public abstract void setUp() throws Exception;

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
		CORE.getPersistence().evictCached(result);
		T deleted = CORE.getPersistence().retrieve(bean.getBizModule(), bean.getBizDocument(), bean.getBizId(), false);

		assertThat(deleted, is(nullValue()));
	}

	@Test
	@SuppressWarnings("boxing")
	public void testFindAll() throws Exception {
		// create the test data
		T b1 = getBean();
		T b2 = getBean();

		CORE.getPersistence().save(b1);
		CORE.getPersistence().save(b2);

		// perform the method under test
		List<T> results = CORE.getPersistence().newDocumentQuery(b1.getBizModule(), b1.getBizDocument()).beanResults();

		// verify the results
		assertThat(results.size(), is(2));
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
			updateAttribute(result, attributeToUpdate);
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

	@SuppressWarnings("unchecked")
	private T updateAttribute(T bean, Attribute attribute) {
		final String name = attribute.getName();
		final AttributeType type = attribute.getAttributeType();

		switch (type) {
			case bool:
				BindUtil.set(bean, name, Boolean.FALSE);
				break;
			case colour:
				BindUtil.set(bean, name, "#FFFFFF");
				break;
			case date:
			case dateTime:
			case time:
			case timestamp:
				Date futureDate = new Date();
				TimeUtil.addHours(futureDate, random.nextInt(10));
				BindUtil.convertAndSet(bean, name, futureDate);
				break;
			case decimal10:
			case decimal2:
			case decimal5:
			case integer:
			case longInteger:
				BindUtil.convertAndSet(bean, name, new Integer((int) Math.random() * 10000));
				break;
			case enumeration:
				// get the current int value of the enum
				Class<Enum<?>> clazz = (Class<Enum<?>>) Binder.getPropertyType(bean, name);
				Object o = Binder.get(bean, name);
				Integer currentValue = null;
				for (int i = 0; i < clazz.getEnumConstants().length; i++) {
					if (clazz.getEnumConstants()[i].equals(o)) {
						currentValue = Integer.valueOf(i);
						break;
					}
				}
				// pick a new random enum
				BindUtil.set(bean, name, randomEnum(clazz, currentValue));
				break;
			case geometry:
				BindUtil.set(bean, name, new GeometryFactory().createPoint(new Coordinate(0, 0)));
				break;
			case id:
				BindUtil.set(bean, name, UUID.randomUUID().toString());
				break;
			case markup:
			case memo:
				BindUtil.set(bean, name, randomString(((int) (Math.random() * 255)) + 1));
				break;
			case text:
				BindUtil.set(bean, name, randomString(((LengthField) attribute).getLength()));
				break;
			case association:
			case collection:
			case content:
			case inverseMany:
			case inverseOne:
				break;
			default:
				break;
		}

		return bean;
	}

	/**
	 * Returns a random value from the enum class
	 * 
	 * @param clazz The enum class
	 * @param currentValue The current int value of the enum so that it is not chosen again
	 * @return A random enum constant
	 */
	@SuppressWarnings("boxing")
	private static <T extends Enum<?>> T randomEnum(Class<T> clazz, Integer currentValue) {
		int x;
		if (currentValue != null) {
			do {
				x = random.nextInt(clazz.getEnumConstants().length);
			} while (x == currentValue);
		} else {
			x = random.nextInt(clazz.getEnumConstants().length);
		}

		return clazz.getEnumConstants()[x];
	}

	private static String randomString(int length) {
		char[] guts = new char[length];
		for (int i = 0; i < length; i++) {
			guts[i] = Character.toChars(65 + (int) (Math.random() * 26))[0];
		}

		return String.valueOf(guts);
	}
}
