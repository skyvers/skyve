package com.drekitek;

import java.util.List;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.RequestScoped;
import javax.faces.context.FacesContext;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.web.faces.FacesAction;
import org.skyve.impl.web.faces.beans.Desktop;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.Persistence;

import modules.admin.domain.Snapshot;

@RequestScoped
@ManagedBean(name = "drektop")
public class DrekitekDesktop extends Desktop {
	private static final long serialVersionUID = 5770380530929766391L;

	private String snapshotScript;
	public String getSnapshotScript() {
		return snapshotScript;
	}

	@Override
	public void preRender() {
		super.preRender();

        final FacesContext fc = FacesContext.getCurrentInstance();
        if (! fc.isPostback()) {
        	new FacesAction<Void>() {
				@Override
				public Void callback() throws Exception {
					createSnapshotScript();
					return null;
				}
        	}.execute();
        }
	}

	private void createSnapshotScript() {
		Persistence p = CORE.getPersistence();
		DocumentQuery q = p.newDocumentQuery(Snapshot.MODULE_NAME, Snapshot.DOCUMENT_NAME);
		q.addBoundProjection(Bean.DOCUMENT_ID);
		q.addBoundProjection(Snapshot.moduleNamePropertyName);
		q.addBoundProjection(Snapshot.queryNamePropertyName);
		q.addBoundProjection(Snapshot.namePropertyName);
		q.addBoundProjection(Snapshot.snapshotPropertyName);
		q.addBoundOrdering(Snapshot.moduleNamePropertyName);
		q.addBoundOrdering(Snapshot.queryNamePropertyName);
		q.addBoundOrdering(Snapshot.ordinalPropertyName);

		List<Object[]> results = q.tupleResults();
		StringBuilder script = new StringBuilder(64 * results.size());
		script.append("isc.BizUtil.snapshots=[");
		for (Object[] row : results) {
			script.append("{").append(Bean.DOCUMENT_ID).append(":'").append(row[0]);
			script.append("',ds:'").append(row[1]).append('_').append(row[2]);
			script.append("',").append(Snapshot.namePropertyName).append(":'").append(row[3]);
			script.append("',").append(Snapshot.snapshotPropertyName).append(':').append(row[4]).append("},");
		}
		if (! results.isEmpty()) {
			script.setLength(script.length() - 1); // remove last comma
		}
		script.append("];");

		snapshotScript = script.toString();
	}
}
