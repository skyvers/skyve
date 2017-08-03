package modules.test;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.impl.util.TestUtil;

import modules.test.domain.UniqueConstraintNonNullable;
import util.AbstractSkyveTest;

public class UniqueConstraintNonNullableTest extends AbstractSkyveTest {

	private UniqueConstraintNonNullable uniqueConstraintNonNullable;

	@Before public void setup() throws Exception {
		uniqueConstraintNonNullable = TestUtil.constructRandomInstance(u, m, ucnn, 0);
	}

	@Test
	public void testSaveSingleInstance() throws Exception {
		CORE.getPersistence().save(uniqueConstraintNonNullable);
	}

	@Test
	public void testSaveTwoDifferentInstances() throws Exception {
		// setup the test data
		UniqueConstraintNonNullable uniqueConstraintNonNullable2 = TestUtil.constructRandomInstance(u, m, ucnn, 0);

		CORE.getPersistence().save(uniqueConstraintNonNullable);

		// validate the test data
		assertThat(uniqueConstraintNonNullable, is(not(uniqueConstraintNonNullable2)));

		// call the method under test
		CORE.getPersistence().save(uniqueConstraintNonNullable2);
	}

	@Test(expected = UniqueConstraintViolationException.class)
	public void testSaveTwoIdenticalInstances() throws Exception {
		// setup the test data
		UniqueConstraintNonNullable uniqueConstraintNonNullable2 = UniqueConstraintNonNullable.newInstance();
		uniqueConstraintNonNullable2.setBooleanFlag(uniqueConstraintNonNullable.getBooleanFlag());
		uniqueConstraintNonNullable2.setEnum3(uniqueConstraintNonNullable.getEnum3());
		uniqueConstraintNonNullable2.setText(uniqueConstraintNonNullable.getText());

		CORE.getPersistence().save(uniqueConstraintNonNullable);

		// validate the test data
		assertThat(uniqueConstraintNonNullable2.getBooleanFlag(), is(uniqueConstraintNonNullable.getBooleanFlag()));
		assertThat(uniqueConstraintNonNullable2.getEnum3(), is(uniqueConstraintNonNullable.getEnum3()));
		assertThat(uniqueConstraintNonNullable2.getText(), is(uniqueConstraintNonNullable.getText()));

		// call the method under test
		CORE.getPersistence().save(uniqueConstraintNonNullable2);
	}

}
