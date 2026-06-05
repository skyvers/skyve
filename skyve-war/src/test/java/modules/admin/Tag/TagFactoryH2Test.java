package modules.admin.Tag;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Test;

import modules.admin.domain.Contact;
import modules.admin.domain.Tag;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class TagFactoryH2Test extends AbstractH2Test {
	@Test
	void crudInstanceDefaultsUploadDocumentToContact() {
		Tag tag = TagFactory.crudInstance();

		assertThat(tag.getUploadModuleName(), is(Contact.MODULE_NAME));
		assertThat(tag.getUploadDocumentName(), is(Contact.DOCUMENT_NAME));
	}

	@Test
	void sailClearsOptionalActionAndUploadFields() {
		Tag tag = new TagFactory().sail();

		assertThat(tag.getOperandTag(), is(nullValue()));
		assertThat(tag.getActionModuleName(), is(nullValue()));
		assertThat(tag.getActionDocumentName(), is(nullValue()));
		assertThat(tag.getUploadModuleName(), is(nullValue()));
		assertThat(tag.getUploadDocumentName(), is(nullValue()));
		assertThat(tag.getDocumentAction(), is(nullValue()));
		assertThat(tag.getDocumentCondition(), is(nullValue()));
		assertThat(tag.getAttributeName(), is(nullValue()));
	}
}
