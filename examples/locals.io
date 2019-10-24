var glob = 50;
{
    var x = 8;
    
    {
        var x = 2;
        x = x + 1;
        {
            var z = 0;
            z = z + 5;
        }
        {
            glob = 60;
        }
    }
    
    print x;
    print glob;
}