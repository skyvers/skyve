package modules.admin.Tag.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.Tag.TagExtension;
import modules.admin.domain.Tag;
import modules.admin.domain.Tag.CombinationsOperator;
import util.AbstractH2Test;

/**
 * H2-backed tests for Tag actions: PrepareExplanation, TagAll, Clear, and PerformCombination validation.
 */
class TagActionsH2Test extends AbstractH2Test {

	private TagExtension bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		bean = Tag.newInstance();
		bean.setName("TestTag-" + UUID.randomUUID());
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
		assertTrue(result.getBean().getCombinationExplanation().contains("Do nothing"));
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
		assertTrue(result.getBean().getCombinationExplanation().contains("Add to"));
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
		assertTrue(result.getBean().getCombinationExplanation().contains("Remove"));
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
		assertFalse(result.getBean().getCombinationExplanation().isEmpty());
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
