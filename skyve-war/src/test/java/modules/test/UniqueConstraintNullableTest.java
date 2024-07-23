package modules.test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.util.test.TestUtil;

import modules.test.domain.UniqueConstraintNullable;

public class UniqueConstraintNullableTest extends AbstractSkyveTest {
	private UniqueConstraintNullable uniqueConstraintNullable;

	@BeforeEach
	public void setup() throws Exception {
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

	@Test
	public void testSaveTwoIdenticalInstancesAllFields() throws Exception {
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
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
		});

		assertThat(ucve.getMessage(), is(notNullValue()));
	}

	/**
	 * This test is ignored because although there is a plan to test for null values inclusive in constraint definitions
	 * it hasn't been implemented yet, so by default null values or partially nulled composite keys are ignored. 
	 */
	@Test
	@Disabled
	public void testSaveTwoIdenticalInstancesWithNull() throws Exception {
		UniqueConstraintViolationException ucve = Assert.assertThrows(UniqueConstraintViolationException.class, () -> {
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
		});

		assertThat(ucve.getMessage(), is(notNullValue()));
	}
}
