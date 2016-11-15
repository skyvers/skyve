package org.skyve.impl.web.faces;

import java.util.List;

import org.primefaces.model.DualListModel;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

/**
 * This class adapts the DomainValue object model for list membership implementation in faces.
 * @author sandsm01
 */
public class DomainValueDualListModel extends DualListModel<DomainValue> {
	private static final long serialVersionUID = -9150794373512375018L;

	public DomainValueDualListModel() {
		super();
	}

	public DomainValueDualListModel(List<DomainValue> source, List<DomainValue> target) {
		super(source, target);
	}
	
	// TODO use the target collection of the DualListModel to adapt the document's bound collection.
}
