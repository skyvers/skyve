package modules.admin.UserDashboard.models;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
public class FavouritesModelTest {

	@Test
	void getDescriptionReturnsExpectedValue() throws Exception {
		FavouritesModel model = new FavouritesModel();
		assertEquals("User Dashboard Tiles", model.getDescription());
	}

	@Test
	void getColumnsReturnsNonEmptyList() throws Exception {
		FavouritesModel model = new FavouritesModel();
		var columns = model.getColumns();
		assertNotNull(columns);
		assertFalse(columns.isEmpty());
	}
}
