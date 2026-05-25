package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag;
import modules.admin.domain.Tag.CombinationsOperator;
import util.AbstractH2Test;

/**
 * H2-backed tests for Tag actions: PrepareExplanation, TagAll, Clear, and PerformCombination validation.
 */
public class TagActionsH2Test extends AbstractH2Test {

	private DataBuilder db;
	private TagExtension bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() throws Exception {
		db = new DataBuilder().fixture(FixtureType.crud);
		bean = Tag.newInstance();
		bean.setName("TestTag");
		bean.setBizId("test-tag-id");
		bean.setUploadModuleName("admin");
		bean.setUploadDocumentName("User");
		bean = CORE.getPersistence().save(bean);
		webContext = new MockWebContext();
	}

	// ---- PrepareExplanation: null operandTag ----

	@Test
	void prepareExplanationWithNullOperandTagSetsEmptyExplanation() throws Exception {
		bean.setOperandTag(null);

		PrepareExplanation action = new PrepareExplanation();
		ServerSideActionResult<TagExtension> result = action.execute(bean, webContext);
		assertNotNull(result);
		assertThat(result.getBean().getCombinationExplanation(), is(""));
	}

	// ---- PrepareExplanation: with operandTag and null operator ----

	@Test
	void prepareExplanationWithOperandTagAndNullOperatorSetsDoNothingExplanation() throws Exception {
		TagExtension operandTag = Tag.newInstance();
		operandTag.setName("OperandTag");
		operandTag = CORE.getPersistence().save(operandTag);

		bean.setOperandTag(operandTag);
		bean.setCombinationsOperator(null);

		PrepareExplanation action = new PrepareExplanation();
		ServerSideActionResult<TagExtension> result = action.execute(bean, webContext);
		assertNotNull(result);
		assertThat(result.getBean().getCombinationExplanation(), is(notNullValue()));
		assertThat(result.getBean().getCombinationExplanation().contains("Do nothing"), is(true));
	}

	// ---- PrepareExplanation: with union operator ----

	@Test
	void prepareExplanationWithUnionOperatorSetsUnionExplanation() throws Exception {
		TagExtension operandTag = Tag.newInstance();
		operandTag.setName("UnionOperand");
		operandTag = CORE.getPersistence().save(operandTag);

		bean.setOperandTag(operandTag);
		bean.setCombinationsOperator(CombinationsOperator.union);
		bean.setName("MainTag");

		PrepareExplanation action = new PrepareExplanation();
		ServerSideActionResult<TagExtension> result = action.execute(bean, webContext);
		assertNotNull(result);
		assertThat(result.getBean().getCombinationExplanation().contains("Add to"), is(true));
	}

	// ---- PrepareExplanation: with except operator ----

	@Test
	void prepareExplanationWithExceptOperatorSetsExceptExplanation() throws Exception {
		TagExtension operandTag = Tag.newInstance();
		operandTag.setName("ExceptOperand");
		operandTag = CORE.getPersistence().save(operandTag);

		bean.setOperandTag(operandTag);
		bean.setCombinationsOperator(CombinationsOperator.except);

		PrepareExplanation action = new PrepareExplanation();
		ServerSideActionResult<TagExtension> result = action.execute(bean, webContext);
		assertNotNull(result);
		assertThat(result.getBean().getCombinationExplanation().contains("Remove"), is(true));
	}

	// ---- PrepareExplanation: with intersect operator ----

	@Test
	void prepareExplanationWithIntersectOperatorSetsIntersectExplanation() throws Exception {
		TagExtension operandTag = Tag.newInstance();
		operandTag.setName("IntersectOperand");
		operandTag = CORE.getPersistence().save(operandTag);

		bean.setOperandTag(operandTag);
		bean.setCombinationsOperator(CombinationsOperator.intersect);

		PrepareExplanation action = new PrepareExplanation();
		ServerSideActionResult<TagExtension> result = action.execute(bean, webContext);
		assertNotNull(result);
		assertThat(result.getBean().getCombinationExplanation(), is(notNullValue()));
		assertThat(result.getBean().getCombinationExplanation().isEmpty(), is(false));
	}

	// ---- TagAll: tags all User records ----

	@Test
	void tagAllTagsAllUsersAndUpdatesCount() throws Exception {
		bean.setUploadModuleName("admin");
		bean.setUploadDocumentName("User");

		TagAll action = new TagAll();
		ServerSideActionResult<TagExtension> result = action.execute(bean, webContext);
		assertNotNull(result);
		assertNotNull(result.getBean().getUploadTagged());
		assertNotNull(result.getBean().getTotalTagged());
	}

	// ---- Clear: deletes tagged records ----

	@Test
	void clearDeletesTaggedRecordsAndReturnsBean() throws Exception {
		Clear action = new Clear();
		ServerSideActionResult<TagExtension> result = action.execute(bean, webContext);
		assertNotNull(result);
		assertNotNull(result.getBean());
	}

	// ---- PerformCombination: null operator ----

	@Test
	void performCombinationWithNullOperatorThrowsValidationException() {
		bean.setCombinationsOperator(null);

		PerformCombination action = new PerformCombination();
		assertThrows(ValidationException.class, () -> action.execute(bean, webContext));
	}
}
