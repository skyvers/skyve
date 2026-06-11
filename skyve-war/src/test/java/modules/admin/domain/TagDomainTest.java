package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import static org.junit.jupiter.api.Assertions.assertNull;

import modules.admin.domain.Tag.CombinationsOperator;
import modules.admin.domain.Tag.FilterAction;
import modules.admin.domain.Tag.FilterOperator;
import util.AbstractH2Test;

/**
 * Tests for the {@link Tag} admin domain bean (persistent).
 * Exercises getter/setter coverage via {@link DataBuilder} and targeted set/get calls.
 */
@SuppressWarnings("static-method")
class TagDomainTest extends AbstractH2Test {

	@Test
	void dataBuilderPopulatesTagBean() {
		Tag bean = new DataBuilder()
				.fixture(FixtureType.crud)
				.build(Tag.MODULE_NAME, Tag.DOCUMENT_NAME);
		assertNotNull(bean);
		assertNotNull(bean.getBizId());
	}

	@Test
	void bizModuleAndDocumentAreCorrect() {
		Tag bean = Tag.newInstance();
		assertEquals(Tag.MODULE_NAME, bean.getBizModule());
		assertEquals(Tag.DOCUMENT_NAME, bean.getBizDocument());
	}

	@Test
	void nameSetAndGet() {
		Tag bean = Tag.newInstance();
		bean.setName("My Tag");
		assertEquals("My Tag", bean.getName());
	}

	@Test
	void visibleSetAndGet() {
		Tag bean = Tag.newInstance();
		bean.setVisible(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getVisible());
	}

	@Test
	void combinationsOperatorSetAndGet() {
		Tag bean = Tag.newInstance();
		bean.setCombinationsOperator(CombinationsOperator.union);
		assertEquals(CombinationsOperator.union, bean.getCombinationsOperator());
	}

	@Test
	void combinationExplanationSetAndGet() {
		Tag bean = Tag.newInstance();
		bean.setCombinationExplanation("Some explanation");
		assertEquals("Some explanation", bean.getCombinationExplanation());
	}

	@Test
	void filterActionSetAndGet() {
		Tag bean = Tag.newInstance();
		bean.setFilterAction(FilterAction.tagRecordsThatMatch);
		assertEquals(FilterAction.tagRecordsThatMatch, bean.getFilterAction());
	}

	@Test
	void uploadModuleAndDocumentNameSetAndGet() {
		Tag bean = Tag.newInstance();
		bean.setUploadModuleName("admin");
		bean.setUploadDocumentName("User");
		bean.setAttributeName("email");
		assertEquals("admin", bean.getUploadModuleName());
		assertEquals("User", bean.getUploadDocumentName());
		assertEquals("email", bean.getAttributeName());
	}

	@Test
	void booleanPropertyFlagsSetAndGet() {
		Tag bean = Tag.newInstance();
		bean.setFileHasHeaders(Boolean.TRUE);
		bean.setNotification(Boolean.FALSE);
		bean.setUnTagSuccessful(Boolean.TRUE);
		assertEquals(Boolean.TRUE, bean.getFileHasHeaders());
		assertEquals(Boolean.FALSE, bean.getNotification());
		assertEquals(Boolean.TRUE, bean.getUnTagSuccessful());
	}

	@Test
	void actionModuleAndDocumentNameSetAndGet() {
		Tag bean = Tag.newInstance();
		bean.setActionModuleName("admin");
		bean.setActionDocumentName("Contact");
		assertEquals("admin", bean.getActionModuleName());
		assertEquals("Contact", bean.getActionDocumentName());
	}

        @Test
        void filterOperatorFromCodeAndFromLocalisedDescription() {
                assertEquals(FilterOperator.equals, FilterOperator.fromCode("equals"));
                assertNull(FilterOperator.fromCode("notexist"));
                assertNull(FilterOperator.fromLocalisedDescription("notexist"));
                assertNotNull(FilterOperator.fromLocalisedDescription(FilterOperator.equals.toLocalisedDescription()));
        }

        @Test
        void filterOperatorToDomainValues() {
                assertNotNull(FilterOperator.toDomainValues());
                assertEquals(3, FilterOperator.toDomainValues().size());
        }

        @Test
        void filterActionFromCodeAndFromLocalisedDescription() {
                assertEquals(FilterAction.tagRecordsThatMatch, FilterAction.fromCode("tag"));
                assertNull(FilterAction.fromCode("notexist"));
                assertNull(FilterAction.fromLocalisedDescription("notexist"));
                assertNotNull(FilterAction.fromLocalisedDescription(FilterAction.tagRecordsThatMatch.toLocalisedDescription()));
        }

        @Test
        void filterActionToDomainValues() {
                assertNotNull(FilterAction.toDomainValues());
                assertEquals(2, FilterAction.toDomainValues().size());
        }

        @Test
        void filterOperatorToCodeAndToDomainValue() {
                assertEquals("equals", FilterOperator.equals.toCode());
                assertNotNull(FilterOperator.equals.toDomainValue());
                assertEquals("equals", FilterOperator.equals.toDomainValue().getCode());
        }

        @Test
        void filterActionToCodeAndToDomainValue() {
                assertEquals("tag", FilterAction.tagRecordsThatMatch.toCode());
                assertNotNull(FilterAction.tagRecordsThatMatch.toDomainValue());
                assertEquals("tag", FilterAction.tagRecordsThatMatch.toDomainValue().getCode());
        }

	@Test
	void combinationsOperatorFromCodeAndFromLocalisedDescription() {
		assertEquals(CombinationsOperator.union, CombinationsOperator.fromCode("Union"));
		assertNull(CombinationsOperator.fromCode("nonexistent"));
		assertNotNull(CombinationsOperator.fromLocalisedDescription(CombinationsOperator.union.toLocalisedDescription()));
		assertNull(CombinationsOperator.fromLocalisedDescription("nonexistent"));
		assertNotNull(CombinationsOperator.toDomainValues());
	}
}
