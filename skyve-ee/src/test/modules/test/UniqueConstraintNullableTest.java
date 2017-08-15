package modules.test;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.impl.util.TestUtil;

import modules.test.domain.UniqueConstraintNullable;

public class UniqueConstraintNullableTest extends AbstractSkyveTest {

	private UniqueConstraintNullable uniqueConstraintNullable;

	@Before public void setup() throws Exception {
		uniqueConstraintNullable = TestUtil.constructRandomInstance(u, m, ucn, 0);
	}

	@Test
	public void testSaveSingleInstance() throws Exception {
		CORE.getPersistence().save(uniqueConstraintNullable);
	}

	@Test
	public void testSaveTwoDifferentInstances() throws Exception {
		// setup the test data
		UniqueConstraintNullable uniqueConstraintNullable2 = TestUtil.constructRandomInstance(u, m, ucn, 0);

		CORE.getPersistence().save(uniqueConstraintNullable);

		// validate the test data
		assertThat(uniqueConstraintNullable, is(not(uniqueConstraintNullable2)));

		// call the method under test
		CORE.getPersistence().save(uniqueConstraintNullable2);
	}

	@Test(expected = UniqueConstraintViolationException.class)
	public void testSaveTwoIdenticalInstancesAllFields() throws Exception {
		// setup the test data
		UniqueConstraintNullable uniqueConstraintNullable2 = UniqueConstraintNullable.newInstance();
		uniqueConstraintNullable2.setBooleanFlag(uniqueConstraintNullable.getBooleanFlag());
		uniqueConstraintNullable2.setEnum3(uniqueConstraintNullable.getEnum3());
		uniqueConstraintNullable2.setText(uniqueConstraintNullable.getText());

		CORE.getPersistence().save(uniqueConstraintNullable);

		// validate the test data
		assertThat(uniqueConstraintNullable2.getBooleanFlag(), is(uniqueConstraintNullable.getBooleanFlag()));
		assertThat(uniqueConstraintNullable2.getEnum3(), is(uniqueConstraintNullable.getEnum3()));
		assertThat(uniqueConstraintNullable2.getText(), is(uniqueConstraintNullable.getText()));

		// call the method under test
		CORE.getPersistence().save(uniqueConstraintNullable2);
	}

	@Test(expected = UniqueConstraintViolationException.class)
	public void testSaveTwoIdenticalInstancesWithNull() throws Exception {
		// setup the test data
		uniqueConstraintNullable.setEnum3(null);

		UniqueConstraintNullable uniqueConstraintNullable2 = UniqueConstraintNullable.newInstance();
		uniqueConstraintNullable2.setBooleanFlag(uniqueConstraintNullable.getBooleanFlag());
		uniqueConstraintNullable2.setEnum3(null);
		uniqueConstraintNullable2.setText(uniqueConstraintNullable.getText());

		CORE.getPersistence().save(uniqueConstraintNullable);

		// validate the test data
		assertThat(uniqueConstraintNullable.getEnum3(), is(nullValue()));
		assertThat(uniqueConstraintNullable2.getBooleanFlag(), is(uniqueConstraintNullable.getBooleanFlag()));
		assertThat(uniqueConstraintNullable2.getEnum3(), is(uniqueConstraintNullable.getEnum3()));
		assertThat(uniqueConstraintNullable2.getText(), is(uniqueConstraintNullable.getText()));

		// call the method under test
		CORE.getPersistence().save(uniqueConstraintNullable2);
	}
}
