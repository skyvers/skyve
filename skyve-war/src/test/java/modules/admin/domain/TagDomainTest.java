package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Tag.CombinationsOperator;
import modules.admin.domain.Tag.FilterAction;
import util.AbstractH2Test;

/**
 * Tests for the {@link Tag} admin domain bean (persistent).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class TagDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesTagBean() throws Exception {
		Tag bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() throws Exception {
		Tag bean = Tag.newInstance();
		assertEquals(Tag.MODULE_NAME, bean.getBizModule());
		assertEquals(Tag.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void nameSetAndGet() throws Exception {
		Tag bean = Tag.newInstance();
		bean.setName("My Tag");
		assertEquals("My Tag", bean.getName());
	}

	@Test
	void visibleSetAndGet() throws Exception {
		Tag bean = Tag.newInstance();
		bean.setVisible(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getVisible());
	}

	@Test
	void combinationsOperatorSetAndGet() throws Exception {
		Tag bean = Tag.newInstance();
		bean.setCombinationsOperator(CombinationsOperator.union);
		assertEquals(CombinationsOperator.union, bean.getCombinationsOperator());
	}

	@Test
	void combinationExplanationSetAndGet() throws Exception {
		Tag bean = Tag.newInstance();
		bean.setCombinationExplanation("Some explanation");
		assertEquals("Some explanation", bean.getCombinationExplanation());
	}

	@Test
	void filterActionSetAndGet() throws Exception {
		Tag bean = Tag.newInstance();
		bean.setFilterAction(FilterAction.tagRecordsThatMatch);
		assertEquals(FilterAction.tagRecordsThatMatch, bean.getFilterAction());
	}

	@Test
	void uploadModuleAndDocumentNameSetAndGet() throws Exception {
		Tag bean = Tag.newInstance();
		bean.setUploadModuleName("admin");
		bean.setUploadDocumentName("User");
		bean.setAttributeName("email");
		assertEquals("admin", bean.getUploadModuleName());
		assertEquals("User", bean.getUploadDocumentName());
		assertEquals("email", bean.getAttributeName());
	}

	@Test
	void booleanPropertyFlagsSetAndGet() throws Exception {
		Tag bean = Tag.newInstance();
		bean.setFileHasHeaders(Boolean.TRUE);
		bean.setNotification(Boolean.FALSE);
		bean.setUnTagSuccessful(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getFileHasHeaders());
		assertEquals(Boolean.FALSE, bean.getNotification());
		assertEquals(Boolean.TRUE, bean.getUnTagSuccessful());
	}

	@Test
	void actionModuleAndDocumentNameSetAndGet() throws Exception {
		Tag bean = Tag.newInstance();
		bean.setActionModuleName("admin");
		bean.setActionDocumentName("Contact");
		assertEquals("admin", bean.getActionModuleName());
		assertEquals("Contact", bean.getActionDocumentName());
	}
}
