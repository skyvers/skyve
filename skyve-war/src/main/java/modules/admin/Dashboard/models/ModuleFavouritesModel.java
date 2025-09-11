package modules.admin.Dashboard.models;

import java.util.ArrayList;
import java.util.List;

import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.TextOutput.Sanitisation;
import org.skyve.metadata.view.model.list.ReferenceListModel;

import modules.admin.domain.Dashboard;
import modules.admin.domain.DashboardTile;

/**
 * Returns a list of "tiles" from the favourites collection on Dashboard.
 */
public class ModuleFavouritesModel extends ReferenceListModel<DashboardTile> {

	private List<MetaDataQueryColumn> columns;

	public ModuleFavouritesModel() throws Exception {
		super(DashboardTile.MODULE_NAME, DashboardTile.DOCUMENT_NAME, Dashboard.favouritesPropertyName);
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		if (this.columns == null) {
			this.columns = new ArrayList<>();

			MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
			column.setName(DashboardTile.tileMarkupPropertyName);
			column.setDisplayName("Markup");
			column.setBinding(DashboardTile.tileMarkupPropertyName);
			// NB BizKey is sanitised in the model's collection.
			column.setEscape(false);
			column.setSanitise(Sanitisation.none);

			this.columns.add(column);
		}

		return this.columns;
	}

	@Override
	public String getDescription() {
		return "Dashboard Dashboard Tiles";
	}
}
