package org.hibernatespatial.readers;

public interface Reader<T> {
	
	public boolean hasNext();
	
	public T next();

	public void close();

}
