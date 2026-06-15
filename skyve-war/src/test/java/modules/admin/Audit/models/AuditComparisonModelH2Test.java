package modules.admin.Audit.models;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.metadata.view.model.comparison.ComparisonComposite;
import org.skyve.metadata.view.model.comparison.ComparisonComposite.Mutation;
import org.skyve.metadata.view.model.comparison.ComparisonProperty;

import modules.admin.domain.Audit;
import modules.admin.domain.Audit.Operation;
import modules.admin.domain.Contact;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;
import util.AbstractH2Test;

class AuditComparisonModelH2Test extends AbstractH2Test {
	private static final String UNKNOWN_MODULE = "missingModule";
	private static final String UNKNOWN_DOCUMENT = "missingDocument";

	@Test
	@SuppressWarnings("static-method")
	void sourceOnlyAuditBuildsAddedRootNodeWithoutMetadata() throws Exception {
		AuditComparisonModel model = new AuditComparisonModel();
		Audit audit = audit(Operation.insert, detail("source-id", "Source key", "\"name\":\"Alpha\""), null);

		ComparisonComposite root = model.getComparisonComposite(audit);

		assertThat(root.getBizId(), is("source-id"));
		assertThat(root.getBusinessKeyDescription(), is("Source key"));
		assertThat(root.getRelationshipDescription(), is(""));
		assertNull(root.getReferenceName());
		assertNull(root.getDocument());
		assertThat(root.getMutation(), is(Mutation.added));
		assertEquals(1, root.getProperties().size());
		ComparisonProperty property = root.getProperties().get(0);
		assertThat(property.getName(), is("name"));
		assertThat(property.getTitle(), is("name"));
		assertThat(property.getWidget(), instanceOf(TextField.class));
		assertThat(property.getNewValue(), is("Alpha"));
		assertNull(property.getOldValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void deleteAuditBuildsDeletedRootNodeWithOldValues() throws Exception {
		AuditComparisonModel model = new AuditComparisonModel();
		Audit audit = audit(Operation.delete, detail("deleted-id", "Deleted key", "\"name\":\"Alpha\""), null);

		ComparisonComposite root = model.getComparisonComposite(audit);

		assertThat(root.getMutation(), is(Mutation.deleted));
		ComparisonProperty property = root.getProperties().get(0);
		assertNull(property.getNewValue());
		assertThat(property.getOldValue(), is("Alpha"));
	}

	@Test
	@SuppressWarnings("static-method")
	void comparisonAuditUpdatesExistingNodeAndAddsComparisonOnlyProperties() throws Exception {
		AuditComparisonModel model = new AuditComparisonModel();
		Audit audit = audit(Operation.update,
				detail("updated-id", "Updated key", "\"name\":\"New\""),
				detail("updated-id", "Updated key", "\"name\":\"Old\",\"removed\":\"Gone\""));

		ComparisonComposite root = model.getComparisonComposite(audit);

		assertThat(root.getMutation(), is(Mutation.updated));
		assertEquals(2, root.getProperties().size());
		ComparisonProperty changed = root.getProperties().get(0);
		assertThat(changed.getName(), is("name"));
		assertThat(changed.getNewValue(), is("New"));
		assertThat(changed.getOldValue(), is("Old"));
		ComparisonProperty removed = root.getProperties().get(1);
		assertThat(removed.getName(), is("removed"));
		assertNull(removed.getNewValue());
		assertThat(removed.getOldValue(), is("Gone"));
	}

	@Test
	@SuppressWarnings("static-method")
	void comparisonAuditWithSameValuesLeavesNodeUnchanged() throws Exception {
		AuditComparisonModel model = new AuditComparisonModel();
		Audit audit = audit(Operation.update,
				detail("same-id", "Same key", "\"name\":\"Same\""),
				detail("same-id", "Same key", "\"name\":\"Same\""));

		ComparisonComposite root = model.getComparisonComposite(audit);

		assertThat(root.getMutation(), is(Mutation.unchanged));
		ComparisonProperty property = root.getProperties().get(0);
		assertThat(property.getNewValue(), is("Same"));
		assertThat(property.getOldValue(), is("Same"));
	}

	@Test
	@SuppressWarnings("static-method")
	void comparisonOnlyRootBindingCreatesDeletedNode() throws Exception {
		AuditComparisonModel model = new AuditComparisonModel();
		Audit audit = audit(Operation.update,
				"{\"items[0]\":" + object("child-id", "Child", "\"description\":\"Child value\"") + "}",
				detail("removed-root", "Removed root", "\"name\":\"Old root\""));

		ComparisonComposite root = model.getComparisonComposite(audit);

		assertThat(root.getBizId(), is("removed-root"));
		assertThat(root.getMutation(), is(Mutation.deleted));
		ComparisonProperty property = root.getProperties().get(0);
		assertNull(property.getNewValue());
		assertThat(property.getOldValue(), is("Old root"));
	}

	@Test
	@SuppressWarnings("static-method")
	void nonRootBindingsAreLinkedAsChildrenWhenMetadataIsUnavailable() throws Exception {
		AuditComparisonModel model = new AuditComparisonModel();
		String sourceDetail = "{"
				+ "\"\":" + object("root-id", "Root", "\"name\":\"Root value\"") + ","
				+ "\"items[0]\":" + object("child-id", "Child", "\"description\":\"Child value\"")
				+ "}";
		Audit audit = audit(Operation.insert, sourceDetail, null);

		ComparisonComposite root = model.getComparisonComposite(audit);

		assertEquals(1, root.getChildren().size());
		ComparisonComposite child = root.getChildren().get(0);
		assertThat(child.getBizId(), is("child-id"));
		assertThat(child.getBusinessKeyDescription(), is("Child"));
		assertThat(child.getMutation(), is(Mutation.added));
		assertThat(child.getProperties().get(0).getName(), is("description"));
	}

	@Test
	@SuppressWarnings("static-method")
	void rootBindingUsesDocumentMetadataWhenAvailable() throws Exception {
		AuditComparisonModel model = new AuditComparisonModel();
		Audit audit = metadataAudit(Operation.insert,
				"{\"\":" + objectWithoutBizKey("user-id", "\"" + UserProxy.userNamePropertyName + "\":\"person@example.com\"") + "}",
				null);

		ComparisonComposite root = model.getComparisonComposite(audit);

		assertThat(root.getBizId(), is("user-id"));
		assertThat(root.getDocument().getName(), is(User.DOCUMENT_NAME));
		assertThat(root.getRelationshipDescription(), is(root.getDocument().getLocalisedSingularAlias()));
		assertThat(root.getBusinessKeyDescription(), is(root.getDocument().getLocalisedSingularAlias()));
		ComparisonProperty property = root.getProperties().get(0);
		assertThat(property.getName(), is(UserProxy.userNamePropertyName));
		assertThat(property.getNewValue(), is("person@example.com"));
	}

	@Test
	@SuppressWarnings("static-method")
	void childBindingUsesReferenceMetadataWhenAvailable() throws Exception {
		AuditComparisonModel model = new AuditComparisonModel();
		String sourceDetail = "{"
				+ "\"\":" + object("user-id", "User key", "\"" + UserProxy.userNamePropertyName + "\":\"person@example.com\"") + ","
				+ "\"" + UserProxy.contactPropertyName + "\":" + object("contact-id", "Contact key", "\"" + Contact.namePropertyName + "\":\"Person\"")
				+ "}";
		Audit audit = metadataAudit(Operation.insert, sourceDetail, null);

		ComparisonComposite root = model.getComparisonComposite(audit);

		assertEquals(1, root.getChildren().size());
		ComparisonComposite child = root.getChildren().get(0);
		assertThat(child.getBizId(), is("contact-id"));
		assertThat(child.getReferenceName(), is(UserProxy.contactPropertyName));
		assertThat(child.getDocument().getName(), is(Contact.DOCUMENT_NAME));
		assertThat(child.getProperties().get(0).getName(), is(Contact.namePropertyName));
	}

	private static Audit audit(Operation operation, String sourceDetail, String comparisonDetail) {
		Audit selected = Audit.newInstance();
		Audit source = version(operation, sourceDetail);
		selected.setSourceVersion(source);
		if (comparisonDetail != null) {
			selected.setComparisonVersion(version(Operation.update, comparisonDetail));
		}
		return selected;
	}

	private static Audit metadataAudit(Operation operation, String sourceDetail, String comparisonDetail) {
		Audit selected = Audit.newInstance();
		Audit source = version(operation, sourceDetail);
		source.setAuditModuleName(User.MODULE_NAME);
		source.setAuditDocumentName(User.DOCUMENT_NAME);
		selected.setSourceVersion(source);
		if (comparisonDetail != null) {
			Audit comparison = version(Operation.update, comparisonDetail);
			comparison.setAuditModuleName(User.MODULE_NAME);
			comparison.setAuditDocumentName(User.DOCUMENT_NAME);
			selected.setComparisonVersion(comparison);
		}
		return selected;
	}

	private static Audit version(Operation operation, String detail) {
		Audit audit = Audit.newInstance();
		audit.setAuditModuleName(UNKNOWN_MODULE);
		audit.setAuditDocumentName(UNKNOWN_DOCUMENT);
		audit.setOperation(operation);
		audit.setAuditDetail(detail);
		return audit;
	}

	private static String detail(String bizId, String bizKey, String properties) {
		return "{\"\":" + object(bizId, bizKey, properties) + "}";
	}

	private static String object(String bizId, String bizKey, String properties) {
		return "{\"" + Bean.DOCUMENT_ID + "\":\"" + bizId + "\",\""
				+ Bean.BIZ_KEY + "\":\"" + bizKey + "\"," + properties + "}";
	}

	private static String objectWithoutBizKey(String bizId, String properties) {
		return "{\"" + Bean.DOCUMENT_ID + "\":\"" + bizId + "\"," + properties + "}";
	}
}
