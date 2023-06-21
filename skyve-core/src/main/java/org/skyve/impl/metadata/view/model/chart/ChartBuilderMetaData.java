package org.skyve.impl.metadata.view.model.chart;

import java.math.BigInteger;
import java.security.MessageDigest;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlElementRefs;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;

import org.skyve.impl.metadata.view.model.ModelMetaData;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.view.model.chart.Bucket;
import org.skyve.persistence.DocumentQuery.AggregateFunction;
import org.skyve.util.JSON;
import org.skyve.util.Util;

@XmlRootElement(namespace = XMLMetaData.VIEW_NAMESPACE)
@XmlType(namespace = XMLMetaData.VIEW_NAMESPACE,
			propOrder = {"title",
							"label",
							"moduleName",
							"documentName",
							"queryName",
							"categoryBinding",
							"categoryBucket",
							"valueBinding",
							"valueFunction",
							"top",
							"order",
							"JFreeChartPostProcessorClassName",
							"primeFacesChartPostProcessorClassName"})
public class ChartBuilderMetaData implements ModelMetaData {
	private static final long serialVersionUID = -6525994383289095450L;

	private String modelName;
	private String title;
	private String label;
	private String moduleName;
	private String documentName;
	private String queryName;
	private String categoryBinding;
	private Bucket categoryBucket;
	private String valueBinding;
	private AggregateFunction valueFunction;
	private ChartBuilderTopMetaData top;
	private ChartBuilderOrderMetaData order;
	private String jFreeChartPostProcessorClassName;
	private String primeFacesChartPostProcessorClassName;
	
	@Override
	@XmlTransient
	public String getModelName() {
		if (modelName == null) {
			try {
				final MessageDigest md = MessageDigest.getInstance("MD5");
				String hash = JSON.marshall(this);
				md.update(hash.getBytes(Util.UTF8));
				modelName = "M" + new BigInteger(1, md.digest()).toString(36);
			}
			catch (Exception e) {
				throw new MetaDataException("ChartBuilderMetaData cannot generate a model name", e);
			}
		}
		return modelName;
	}

	public String getTitle() {
		return title;
	}
	@XmlAttribute(required = false)
	public void setTitle(String title) {
		this.title = UtilImpl.processStringValue(title);
	}

	public String getLabel() {
		return label;
	}
	@XmlAttribute(required = true)
	public void setLabel(String label) {
		this.label = UtilImpl.processStringValue(label);
	}
	
	public String getModuleName() {
		return moduleName;
	}
	@XmlAttribute(required = true)
	public void setModuleName(String moduleName) {
		this.moduleName = UtilImpl.processStringValue(moduleName);
	}
	
	public String getDocumentName() {
		return documentName;
	}
	@XmlAttribute
	public void setDocumentName(String documentName) {
		this.documentName = UtilImpl.processStringValue(documentName);
	}

	public String getQueryName() {
		return queryName;
	}
	@XmlAttribute
	public void setQueryName(String queryName) {
		this.queryName = UtilImpl.processStringValue(queryName);
	}

	public String getCategoryBinding() {
		return categoryBinding;
	}
	@XmlAttribute(required = true)
	public void setCategoryBinding(String categoryBinding) {
		this.categoryBinding = UtilImpl.processStringValue(categoryBinding);
	}

	public Bucket getCategoryBucket() {
		return categoryBucket;
	}
	
	@XmlElementRefs({@XmlElementRef(type = NoBucketMetaData.class),
						@XmlElementRef(type = NumericMultipleBucketMetaData.class),
						@XmlElementRef(type = NumericRangeBucketMetaData.class),
						@XmlElementRef(type = TemporalBucketMetaData.class),
						@XmlElementRef(type = TextLengthBucketMetaData.class),
						@XmlElementRef(type = TextStartsWithBucketMetaData.class)})
	public void setCategoryBucket(Bucket categoryBucket) {
		this.categoryBucket = categoryBucket;
		if (categoryBucket instanceof NumericRangeBucketMetaData) {
			((NumericRangeBucketMetaData) categoryBucket).convert();
		}
	}

	public String getValueBinding() {
		return valueBinding;
	}
	@XmlAttribute(required = true)
	public void setValueBinding(String valueBinding) {
		this.valueBinding = UtilImpl.processStringValue(valueBinding);
	}
	
	public AggregateFunction getValueFunction() {
		return valueFunction;
	}
	@XmlAttribute
	public void setValueFunction(AggregateFunction valueFunction) {
		this.valueFunction = valueFunction;
	}

	public ChartBuilderTopMetaData getTop() {
		return top;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setTop(ChartBuilderTopMetaData top) {
		this.top = top;
	}
	
	public ChartBuilderOrderMetaData getOrder() {
		return order;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setOrder(ChartBuilderOrderMetaData order) {
		this.order = order;
	}

	public String getJFreeChartPostProcessorClassName() {
		return jFreeChartPostProcessorClassName;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setJFreeChartPostProcessorClassName(String jFreeChartPostProcessorClassName) {
		this.jFreeChartPostProcessorClassName = UtilImpl.processStringValue(jFreeChartPostProcessorClassName);
	}

	public String getPrimeFacesChartPostProcessorClassName() {
		return primeFacesChartPostProcessorClassName;
	}
	@XmlElement(namespace = XMLMetaData.VIEW_NAMESPACE)
	public void setPrimeFacesChartPostProcessorClassName(String primeFacesChartPostProcessorClassName) {
		this.primeFacesChartPostProcessorClassName = UtilImpl.processStringValue(primeFacesChartPostProcessorClassName);
	}
}
