package org.skyve.metadata.view.model.list;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

import util.AbstractH2Test;

@SuppressWarnings({"static-method", "boxing"})
class RelationTreeModelH2Test extends AbstractH2Test {
	@Test
	void columnsAreCreatedOnceWithRelatedDisplayName() {
		RelationTreeModel<Bean> model = new TestRelationTreeModel();

		java.util.List<MetaDataQueryColumn> columns = model.getColumns();

		assertThat(columns.size(), is(1));
		assertThat(columns.get(0).getDisplayName(), is("Related"));
		assertThat(model.getColumns(), is(columns));
	}

	@Test
	void rowsStartEmptyAndFilterCanBeCreated() {
		RelationTreeModel<Bean> model = new TestRelationTreeModel("Generic");

		assertThat(model.getRows().isEmpty(), is(true));
		assertThat(model.newFilter() instanceof RelationTreeModelFilter, is(true));
	}

	@Test
	void mutationMethodsAreNotImplemented() {
		RelationTreeModel<Bean> model = new TestRelationTreeModel();

		assertThrows(IllegalStateException.class, () -> model.update("id", new TreeMap<>()));
		assertThrows(IllegalStateException.class, () -> model.remove("id"));
	}

	private static final class TestRelationTreeModel extends RelationTreeModel<Bean> {
		private TestRelationTreeModel(String... stopDocuments) {
			super(stopDocuments);
		}

		@Override
		public String getDescription() {
			return "TestRelationTreeModel";
		}
	}
}
