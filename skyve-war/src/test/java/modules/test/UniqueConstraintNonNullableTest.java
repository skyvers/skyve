package modules.test;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.UniqueConstraintViolationException;
import org.skyve.util.test.TestUtil;

import modules.test.domain.UniqueConstraintNonNullable;

@SuppressWarnings({ "java:S5778", "java:S1130", "java:S1854" })
class UniqueConstraintNonNullableTest extends AbstractSkyveTest {

	private UniqueConstraintNonNullable uniqueConstraintNonNullable;

	@BeforeEach
	void setup() throws Exception {
		uniqueConstraintNonNullable = TestUtil.constructRandomInstance(u, m, ucnn, 0);
	}

	@Test
	void testSaveSingleInstance() throws Exception {
		Assertions.assertDoesNotThrow(() -> CORE.getPersistence().save(uniqueConstraintNonNullable));
	}

	@Test
	void testSaveTwoDifferentInstances() throws Exception {
		// setup the test data
		UniqueConstraintNonNullable uniqueConstraintNonNullable2 = TestUtil.constructRandomInstance(u, m, ucnn, 0);

		CORE.getPersistence().save(uniqueConstraintNonNullable);

		// validate the test data
		assertThat(uniqueConstraintNonNullable, is(not(uniqueConstraintNonNullable2)));

		// call the method under test
		CORE.getPersistence().save(uniqueConstraintNonNullable2);
	}

	@Test
	void testSaveTwoIdenticalInstances() throws Exception {
		UniqueConstraintViolationException ucve = Assertions.assertThrows(UniqueConstraintViolationException.class, () -> {
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
		});

		assertTrue(ucve.getMessages().size() > 0);
	}
}
