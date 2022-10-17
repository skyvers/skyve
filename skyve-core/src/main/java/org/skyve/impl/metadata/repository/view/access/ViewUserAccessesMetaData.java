package org.skyve.impl.metadata.repository.view.access;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.util.XMLMetaData;

@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE, name = "accesses")
public class ViewUserAccessesMetaData {
	private List<ViewUserAccessMetaData> accesses = new ArrayList<>();
	private boolean generate = true;
	
	@XmlElementRefs({@XmlElementRef(type = ViewSingularUserAccessMetaData.class), 
						@XmlElementRef(type = ViewDocumentAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ViewQueryAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ViewModelAggregateUserAccessMetaData.class),
						@XmlElementRef(type = ViewPreviousCompleteUserAccessMetaData.class)})
	public List<ViewUserAccessMetaData> getAccesses() {
		return accesses;
	}
	
	public boolean isGenerate() {
		return generate;
	}

	@XmlAttribute(required = true)
	public void setGenerate(boolean generate) {
		this.generate = generate;
	}
}
