package org.skyve.metadata.view.model.map;

import org.skyve.domain.Bean;
import org.skyve.metadata.MetaData;

import com.vividsolutions.jts.geom.Envelope;

public abstract class MapModel<T extends Bean> implements MetaData {
	private static final long serialVersionUID = 3483411029192491351L;

	private T bean;
	public T getBean() {
		return bean;
	}
	public void setBean(T bean) {
		this.bean = bean;
	}
	
	public abstract MapResult getResult(Envelope mapExtents) throws Exception;
}
