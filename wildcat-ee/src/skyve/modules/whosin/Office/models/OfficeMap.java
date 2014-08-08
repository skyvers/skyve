package modules.whosin.Office.models;

import java.util.ArrayList;
import java.util.List;

import modules.whosin.domain.Office;
import modules.whosin.domain.Staff;
import modules.whosin.domain.Staff.Status;

import org.skyve.CORE;
import org.skyve.metadata.view.model.map.MapFeature;
import org.skyve.metadata.view.model.map.MapItem;
import org.skyve.metadata.view.model.map.MapModel;
import org.skyve.metadata.view.model.map.MapResult;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.Geometry;

public class OfficeMap extends MapModel<Office> {
	private static final long serialVersionUID = 7880044512360465355L;

	@Override
	public MapResult getResult(Envelope mapExtents) throws Exception {
		Office office = getBean();		
		
		List<MapItem> items = new ArrayList<>();

			// add the office feature
		Geometry boundary = office.getBoundary();
		if (boundary != null) {
			MapItem item = new MapItem();
			item.setBizId(office.getBizId());
			item.setModuleName(office.getBizModule());
			item.setDocumentName(office.getBizDocument());
			item.setInfoMarkup(office.getBizKey());
			
			MapFeature feature = new MapFeature();
			feature.setGeometry(office.getBoundary());
			feature.setFillColour("#009900");
			feature.setFillOpacity("0.6");
			feature.setStrokeColour("#00FF00");
			item.getFeatures().add(feature);
			items.add(item);
		}
		
		// add the staff features
		if (office.isPersisted()) {
			Persistence p = CORE.getPersistence();
			DocumentQuery q = p.newDocumentQuery(Staff.MODULE_NAME, Staff.DOCUMENT_NAME);
			q.getFilter().addEquals(Staff.baseOfficePropertyName, office);
			List<Staff> staff = p.retrieve(q);
			for (Staff member : staff) {
				MapItem item = new MapItem();
				item.setBizId(member.getBizId());
				item.setModuleName(member.getBizModule());
				item.setDocumentName(member.getBizDocument());
				
				Status memberStatus = member.getStatus();
				StringBuilder markup = new StringBuilder(64);
				markup.append(member.getContact().getName());
				if (memberStatus != null) {
					markup.append("<br/>").append(memberStatus.toDescription());
				}
				item.setInfoMarkup(markup.toString());
				
				MapFeature feature = new MapFeature();
				feature.setGeometry(member.getLocation());
				item.getFeatures().add(feature);
				
				items.add(item);
			}
		}
		
		return new MapResult(items, null);
	}
}
