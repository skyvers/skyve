package modules.whosin.Staff.models;

import java.util.ArrayList;
import java.util.List;

import org.locationtech.jts.geom.Geometry;
import org.skyve.CORE;
import org.skyve.metadata.view.model.map.MapFeature;
import org.skyve.metadata.view.model.map.MapItem;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.metadata.view.model.map.MapResult;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.whosin.domain.Staff;
import modules.whosin.domain.Staff.Status;

public class AllStaffMap extends MapModel<Staff> {
	@Override
	public MapResult getResult(Geometry mapBounds) throws Exception {

		List<MapItem> items = new ArrayList<>();

		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(Staff.MODULE_NAME, Staff.DOCUMENT_NAME);

		List<Staff> staff = q.beanResults();
		for (Staff member : staff) {
			if (mapBounds.intersects(member.getLocation())) {
				MapItem item = new MapItem();
				item.setBizId(member.getBizId());
				item.setModuleName(member.getBizModule());
				item.setDocumentName(member.getBizDocument());

				Status memberStatus = member.getStatus();
				StringBuilder markup = new StringBuilder(64);
				markup.append(member.getContact().getName());
				if (memberStatus != null) {
					markup.append("<br/>").append(memberStatus.toLocalisedDescription());
				}
				item.setInfoMarkup(markup.toString());

				MapFeature feature = new MapFeature();
				feature.setGeometry(member.getLocation());
				feature.setIconRelativeFilePath("icons/document/user16.png");
				feature.setIconAnchorX(Integer.valueOf(8));
				feature.setIconAnchorY(Integer.valueOf(8));
				item.getFeatures().add(feature);

				items.add(item);
			}
		}

		return new MapResult(items, null);
	}
}
