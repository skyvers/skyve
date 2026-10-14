package modules.kitchensink.ConditionsFixture.models;

import java.util.ArrayList;

import org.locationtech.jts.geom.Geometry;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.metadata.view.model.map.MapResult;

import modules.kitchensink.domain.ConditionsFixture;

/**
 * Supplies an empty map, which is all the conditions fixture needs to put a condition on a map.
 */
public class ConditionsMapModel extends MapModel<ConditionsFixture> {
	@Override
	public MapResult getResult(Geometry mapBounds) throws Exception {
		return new MapResult(new ArrayList<>(), null);
	}
}
