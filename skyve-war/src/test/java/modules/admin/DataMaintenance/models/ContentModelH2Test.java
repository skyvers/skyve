package modules.admin.DataMaintenance.models;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.domain.Bean;

import modules.admin.domain.Content;
import util.AbstractH2Test;

class ContentModelH2Test extends AbstractH2Test {
	@Test
	@SuppressWarnings("static-method")
	void postConstructPopulatesDrivingDocumentColumnsAndProjections() {
		ContentModel model = new ContentModel();

		model.postConstruct(CORE.getCustomer(), false);

		assertThat(model.getDescription(), is("All Content"));
		assertThat(model.getDrivingDocument().getName(), is(Content.DOCUMENT_NAME));
		assertThat(model.getColumns().size(), is(8));
		assertThat(model.getColumns().get(0).getBinding(), is(Content.customerNamePropertyName));
		assertThat(model.getColumns().get(1).getBinding(), is(Content.moduleNamePropertyName));
		assertThat(model.getColumns().get(2).getBinding(), is(Content.documentNamePropertyName));
		assertThat(model.getColumns().get(3).getBinding(), is(Content.attributeNamePropertyName));
		assertThat(model.getColumns().get(4).getBinding(), is(Content.contentBizIdPropertyName));
		assertThat(model.getColumns().get(5).getBinding(), is(Content.contentIdPropertyName));
		assertThat(model.getColumns().get(6).getBinding(), is(Content.lastModifiedPropertyName));
		assertThat(model.getColumns().get(7).getBinding(), is(Content.contentPropertyName));
		assertThat(model.getProjections().contains(Bean.DOCUMENT_ID), is(true));
		assertThat(model.getProjections().contains(Content.contentPropertyName), is(true));
		assertThat(model.getProjections().contains(Content.lastModifiedPropertyName), is(true));
		assertNull(model.getFilter());
		assertNull(model.newFilter());
		model.putParameter("ignored", "ignored");
	}

	@Test
	@SuppressWarnings("static-method")
	void unsupportedListModelOperationsThrow() throws Exception {
		ContentModel model = new ContentModel();

		assertThrows(IllegalStateException.class, model::iterate);
		assertThrows(IllegalStateException.class, () -> model.update("id", new TreeMap<>()));
		assertThrows(IllegalStateException.class, () -> model.remove("id"));
	}
}
