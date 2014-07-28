package org.hibernatespatial.pojo;

import java.security.ProtectionDomain;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtField;
import javassist.CtMethod;
import javassist.CtNewMethod;

public class FeatureClassGenerator {

	private final String packageName;
	private final NamingStrategy naming;
	private final static ClassPool pool = ClassPool.getDefault();

	public FeatureClassGenerator(String packageName, NamingStrategy naming) {
		this.packageName = packageName;
		this.naming = naming;
	}

	public Class<?> generate(ClassInfo classInfo) {

		try {
			String classname = packageName + "." + classInfo.getClassName();
			CtClass pojo = pool.makeClass(classname);
			for (AttributeInfo ai : classInfo.getAttributes()) {
				CtField field = createField(ai, pojo);
				CtMethod getter = createGetterMethod(field);
				CtMethod setter = createSetterMethod(field);
				pojo.addField(field);
				pojo.addMethod(getter);
				pojo.addMethod(setter);
			}
			ProtectionDomain pd = pojo.getClass().getProtectionDomain();
			ClassLoader cl = Thread.currentThread().getContextClassLoader();
			Class<?> clazz = pojo.toClass(cl, pd);
			pojo.detach();
			return clazz;
		} catch (CannotCompileException e){
			e.printStackTrace(); //TODO -- provide proper logging
			throw new RuntimeException("Problem generating class for table " + classInfo.getTableName(), e);
		}
		
	}

	private CtMethod createGetterMethod(CtField field) throws CannotCompileException {
		String fn = field.getName();
		return CtNewMethod.getter(this.naming.createGetterName(fn), field);
	}

	private CtMethod createSetterMethod(CtField field)
			throws CannotCompileException {
		String fn = field.getName();
		return CtNewMethod.setter(naming.createSetterName(fn), field);
	}

	private CtField createField(AttributeInfo ai, CtClass declaring)
			throws CannotCompileException {
			CtField f = new CtField(ai.getCtClass(), ai.getFieldName(), declaring);
		return f;
	}

}
