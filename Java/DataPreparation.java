package com.example;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.JSONParser;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

public class DataPreparation {
    public static void main(String args[]) throws IOException, SQLException {

        int Service=0,Cleanliness=0,Value=0,SleepQuality=0,Rooms=0,Location=0;
        float Overall=0.0f;

        String AuthorLocation="",Title="",Author="",ReviewID="",Content="",Date="";

        PrintStream hname = new PrintStream(new File("HotelNames.txt"));

        String Name="", Address="";
        String Locality="";
        String Name1="";
        JSONParser parser = new JSONParser();

        Connection myConn = null;
        Statement myStmt = null;
        ResultSet myRs = null;

        try {
            // 1. Get a connection to database
            myConn = DriverManager.getConnection("jdbc:mysql://localhost:3306/demo", "root", "root");
            // 2. Create a statement
            myStmt = myConn.createStatement();
        }catch (Exception exc) {
            exc.printStackTrace();
        }



        File folder = new File("E:\\FYP\\JSON");
        File[] listOfFiles = folder.listFiles();
        for (int j = 0; j < listOfFiles.length; j++) {
            File file = listOfFiles[j];
            try {
                Object obj = parser.parse(new FileReader(file));
                JSONObject jsonObject = (JSONObject) obj;
                JSONObject HotelInfo = (JSONObject) jsonObject.get("HotelInfo");
                Name = (String) HotelInfo.get("Name");
                Address = (String) HotelInfo.get("Address");
                Locality = Address.substring(Address.indexOf("v:locality\">") + 12, Address.indexOf("</span>, <span property=\"v:region"));

                try {
                    PreparedStatement create = myConn.prepareStatement("CREATE TABLE IF NOT EXISTS HotelLists " +
                            "(" +
                            "Hotel varchar(255)," +
                            "Locality varchar(255)" +
                            ")");
                    create.executeUpdate();
                    System.out.println("Hotel List Table Created");
                }catch (Exception exc) {
                    exc.printStackTrace();
                }

                try {


                    PreparedStatement inserts= myConn.prepareStatement("INSERT INTO "+"HotelLists"+" VALUES ("+
                            "'"+Name+
                            "' ,'"+Locality+
                            "')");
                    inserts.executeUpdate();
                }
                catch (Exception exc) {
                    System.out.println(""+Title);
                    exc.printStackTrace();
                }

                Name1=Name;
                Name=Name.replace(' ', '_');
                Name=Name.replace('-', '_');
                if(Name.length()>30) {
                    Name = Name.substring(0, 30);
                }

                try {
                    PreparedStatement create = myConn.prepareStatement("CREATE TABLE IF NOT EXISTS " + Name +
                            "(" +
                            "Service int," +
                            "Cleanliness int," +
                            "Value int," +
                            "SleepQuality int," +
                            "Rooms int," +
                            "Location int," +
                            "Overall decimal(3, 2)," +
                            "AuthorLocation varchar(255)," +
                            "Title varchar(255)," +
                            "Author varchar(255)," +
                            "Content varchar(5000)," +
                            "ReviewID varchar(255)," +
                            "Date varchar(255)" +
                            ")");
                    create.executeUpdate();
                    System.out.println(Name + " Table Created");
                }catch (Exception exc) {
                    exc.printStackTrace();
                }

                PrintStream fout = new PrintStream(new File(Name+".txt"));


                hname.println(Name1);


                JSONArray reviews = (JSONArray) jsonObject.get("Reviews");


                for (int i = 0; i < reviews.size(); i++) {
                    JSONObject c = (JSONObject) reviews.get(i);

                    JSONObject Ratings = (JSONObject) c.get("Ratings");

                    try {
                        Service = Integer.parseInt((String) Ratings.get("Service"));
                    }catch (NumberFormatException e){
                        Service=0;
                    }
                    try {
                        Cleanliness = Integer.parseInt((String) Ratings.get("Cleanliness"));
                    }catch (NumberFormatException e){
                        Cleanliness=0;
                    }
                    try {
                        Overall = Float.parseFloat((String) Ratings.get("Overall"));
                    }catch (NumberFormatException e){
                        Overall=0.0f;
                    }
                    try {
                        Value = Integer.parseInt((String) Ratings.get("Value"));
                    }catch (NumberFormatException e){
                        Value=0;
                    }
                    try {
                        SleepQuality = Integer.parseInt((String) Ratings.get("Sleep Quality"));
                    }catch (NumberFormatException e){
                        SleepQuality=0;
                    }
                    try {
                        Rooms = Integer.parseInt((String) Ratings.get("Rooms"));
                    }catch (NumberFormatException e){
                        Rooms=0;
                    }
                    try {
                        Location = Integer.parseInt((String) Ratings.get("Location"));
                    }catch (NumberFormatException e){
                        Location=0;
                    }
                    AuthorLocation = (String) c.get("AuthorLocation");
                    AuthorLocation=AuthorLocation.replaceAll("[^a-zA-Z0-9,.!\\s]", "");
                    Title = (String) c.get("Title");
                    Title=Title.replaceAll("[^a-zA-Z0-9,.!\\s]", "");
                    Author = (String) c.get("Author");
                    Author=Author.replaceAll("[^a-zA-Z0-9,.!\\s]", "");
                    ReviewID = (String) c.get("ReviewID");
                    ReviewID=ReviewID.replaceAll("[^a-zA-Z0-9,.!\\s]", "");
                    Content = (String) c.get("Content");
                    Content=Content.replaceAll("[^a-zA-Z0-9,.!\\s]", "");
                    Date = (String) c.get("Date");
                    Date=Date.replaceAll("[^a-zA-Z0-9,.!\\s]", "");

                    try {


                        PreparedStatement inserts= myConn.prepareStatement("INSERT INTO "+Name+" VALUES ("+Service+
                                " ,"+Cleanliness+
                                " ,"+Value+
                                " ,"+SleepQuality+
                                " ,"+Rooms+
                                " ,"+Location+
                                " ,"+Overall+
                                " ,'"+AuthorLocation+
                                "' ,'"+Title+
                                "' ,'"+Author+
                                "' ,'"+Content+
                                "' ,'"+ReviewID+
                                "' ,'"+Date+
                                "')");
                        inserts.executeUpdate();
                    }
                    catch (Exception exc) {
                        System.out.println(""+Title);
                        exc.printStackTrace();
                    }

                    try
                    {
                        fout.println(Content);

                    }catch (Exception e){
                        System.out.println(e);
                    }

                }

                fout.close();

            } catch (FileNotFoundException e) {
                e.printStackTrace();
            } catch (IOException e) {
                e.printStackTrace();
            }catch (org.json.simple.parser.ParseException e) {
                e.printStackTrace();
            }
            finally {

                System.out.println("All rows added in "+Name);


            }
        }

        if (myRs != null) {
            myRs.close();
        }

        if (myStmt != null) {
            myStmt.close();
        }

        if (myConn != null) {
            myConn.close();
        }

        hname.close();




    }
}
