package modules.admin.UserDashboard.models;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.model.list.ReferenceListModel;

import modules.admin.domain.Generic;
import modules.admin.domain.UserDashboard;

/**
 * Returns a list of "tiles" from the favourites collection on UserDashboard.
 */
public class FavouritesModel extends ReferenceListModel<Generic> {

	private List<MetaDataQueryColumn> columns;

	public FavouritesModel() throws Exception {
		super(Generic.MODULE_NAME, Generic.DOCUMENT_NAME, UserDashboard.favouritesPropertyName);
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		if (columns == null) {
			columns = new ArrayList<>();

			MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
			column.setName(Generic.markup1PropertyName);
			column.setDisplayName("Markup");
			column.setBinding(Generic.markup1PropertyName);
			// NB BizKey is sanitised in the model's collection.
			column.setEscape(false);
			column.setSanitise(Sanitisation.none);

			columns.add(column);
		}

		return columns;
	}

	@Override
	public String getDescription() {
		return "User Dashboard Tiles";
	}
}
