package org.skyve.metadata.module.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.module.JobMetaDataImpl;

/**
 * Tests for {@link FluentJob}: constructors, setters, and {@code from()}.
 */
@SuppressWarnings("static-method")
class FluentJobTest {

	@Test
	void defaultConstructorCreatesInstance() {
		assertNotNull(new FluentJob().get());
	}

	@Test
	void wrappingConstructorPreservesInstance() {
		JobMetaDataImpl job = new JobMetaDataImpl();
		FluentJob fj = new FluentJob(job);
		assertEquals(job, fj.get());
	}

	@Test
	void nameSetsValue() {
		FluentJob fj = new FluentJob().name("myJob");
		assertEquals("myJob", fj.get().getName());
	}

	@Test
	void displayNameSetsValue() {
		FluentJob fj = new FluentJob().displayName("My Job");
		assertEquals("My Job", fj.get().getDisplayName());
	}

	@Test
	void descriptionSetsValue() {
		FluentJob fj = new FluentJob().description("Does stuff");
		assertEquals("Does stuff", fj.get().getDescription());
	}

	@Test
	void classNameSetsValue() {
		FluentJob fj = new FluentJob().className("com.example.MyJob");
		assertEquals("com.example.MyJob", fj.get().getClassName());
	}

	@Test
	void fromCopiesAllFields() {
		JobMetaDataImpl source = new JobMetaDataImpl();
		source.setName("srcJob");
		source.setDisplayName("Source Job");
		source.setDescription("Source desc");
		source.setClassName("com.example.Source");

		FluentJob copy = new FluentJob().from(source);

		assertEquals("srcJob", copy.get().getName());
		assertEquals("Source Job", copy.get().getDisplayName());
		assertEquals("Source desc", copy.get().getDescription());
		assertEquals("com.example.Source", copy.get().getClassName());
	}
}
