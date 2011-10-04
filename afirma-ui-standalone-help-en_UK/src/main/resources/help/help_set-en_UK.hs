<?xml version="1.0" encoding='ISO-8859-1' ?>
<!DOCTYPE helpset
  PUBLIC "-//Sun Microsystems Inc.//DTD JavaHelp HelpSet Version 1.0//EN"
         "http://java.sun.com/products/javahelp/helpset_1_0.dtd">
 

<helpset version="1.0">
        <title>Ejemplo ayuda JavaHelp</title>
        <maps>
                <!-- Pagina por defecto al mostrar la ayuda -->
                <homeID>aplicacion</homeID>
                <!-- Que mapa deseamos -->
                <mapref location="map_file-en.jhm"/>
        </maps>

        <!-- Las Vistas que deseamos mostrar en la ayuda -->
        <view>
                <name>Tabla Contenidos</name>
                <label>Tabla de contenidos</label>
                <type>javax.help.TOCView</type>
                <data>toc-en.xml</data>
        </view>

  <view>
    <name>Index</name>
    <label>Index</label>
    <type>javax.help.IndexView</type>
    <data>index-en.xml</data>
  </view>
  
  <view>
    <name>Search</name>
    <label>Search</label>
    <type>javax.help.SearchView</type>
    <data engine="com.sun.java.help.search.DefaultSearchEngine">
      JavaHelpSearch
    </data>
  </view>
  
</helpset>

