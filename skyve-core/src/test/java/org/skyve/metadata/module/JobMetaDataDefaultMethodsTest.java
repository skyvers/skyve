package org.skyve.metadata.module;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.JobMetaDataImpl;

/**
 * Tests for default methods on the JobMetaData interface.
 */
@SuppressWarnings("static-method")
class JobMetaDataDefaultMethodsTest {

	@Test
	void getLocalisedDisplayNameReturnsNullForNullDisplayName() {
		JobMetaDataImpl mockJob = new JobMetaDataImpl();
		mockJob.setDisplayName(null);
		// Util.i18n(null) returns null
		assertThat(mockJob.getLocalisedDisplayName(), nullValue());
	}

	@Test
	void getLocalisedDisplayNameReturnsNonNullForRealName() {
		JobMetaDataImpl mockJob = new JobMetaDataImpl();
		mockJob.setDisplayName("Generate Report");
		assertThat(mockJob.getLocalisedDisplayName(), notNullValue());
	}

	@Test
	void getLocalisedDescriptionReturnsNullForNullDescription() {
		JobMetaDataImpl mockJob = new JobMetaDataImpl();
		mockJob.setDescription(null);
		// Util.i18n(null) returns null
		assertThat(mockJob.getLocalisedDescription(), nullValue());
	}

	@Test
	void getLocalisedDescriptionReturnsNonNullForRealDescription() {
		JobMetaDataImpl mockJob = new JobMetaDataImpl();
		mockJob.setDescription("Generates a weekly report");
		assertThat(mockJob.getLocalisedDescription(), notNullValue());
	}
}
